package scalapplcodefest.sbt

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
class MPGraphReplacerNew(val env: GeneratorEnvironment) extends CodeStringReplacer with WolfePatterns {

  import env._
  import global._

  //the mpGraph variable name
  val mpGraphName = "_mpGraph"


  def whileLoopRange(indexName: String, to: String, from: String = "0")(body: String) = {
    s"{ var $indexName = 0;\n while ($indexName < $to) \n{ $body;\n $indexName += 1}}"
  }
  def block(statements: Seq[String]) = statements.mkString("{\n", ";\n", "\n}")

  def treeToName(tree: Tree) = {
    val string = tree.toString()
    string.replaceAll("[\\.\\[\\]\\(\\)\\,]", "_").replaceAll("\t\f\n\r", "")
  }

  //iterate over domains to create settings and table values
  def settingsLoop(nodeDefs: List[NodeDefField], factor: FactorDef,
                   dataArg: Symbol, fieldName2nodeDef: Map[TermName, NodeDefField]): String = {
    import factor._
    nodeDefs match {
      case Nil =>
        //call potential with current arguments
        val settingDef = s"val _setting = ${factor.nodes.map(n => s"${n.indexName}").mkString("Array(", ",", ")")}"
        val updateSettingTable = s"$settingsName($settingIndexName) = _setting"
        //replace original selects in arg tree with dom(index) expressions
        val substituted = transform(factor.potential, {
          case s@Select(i@Ident(_), _) if i.symbol == dataArg =>
            val node = fieldName2nodeDef(s.name)
            node.condition match {
              case Some(value) => value
              case _ => Ident(node.valueName)
            }
        })
        val scoreDef = s"val _score = math.log($substituted)"
        val updateTable = s"$tableName($settingIndexName) = _score"
        val settingInc = s"$settingIndexName += 1"
        Seq(settingDef, updateSettingTable, scoreDef, updateTable, settingInc).mkString(";\n")
      case node :: tail =>
        val iName = node.indexName
        val vName = node.valueName
        val dName = node.domName
        s"{ var $iName = 0; " +
        s"while ($iName < ${node.domName}.length) " +
        s"{val $vName = $dName($iName); ${settingsLoop(tail, factor, dataArg, fieldName2nodeDef)};$iName += 1}}"
    }
  }


  case class NodeDefField(field: ValDef, dom: Tree, nodeName: String, domName: String, condition: Option[Tree] = None) {
    val fieldName = field.name.encoded
    val indexName = s"_index_$fieldName"
    val valueName = s"_value_$fieldName"
    val domDef    = s"val $domName = $dom.toArray"
    val nodeDef   = s"val $nodeName = $mpGraphName.addNode($domName.length)"
  }

  case class FactorDef(factorIndex: Int, potential: Tree, origNodes: List[NodeDefField]) {
    val nodes             = origNodes.filter(_.condition.isEmpty)
    val settingsName      = s"_settings_$factorIndex"
    val tableName         = s"_table_$factorIndex"
    val dimName           = s"_dim_$factorIndex"
    val settingsCount     = nodes.map(_.domName + ".length").mkString(" * ")
    val settingsCountName = s"_settingsCount_$factorIndex"
    val settingsCountDef  = s"val $settingsCountName = $settingsCount"
    val settingsDef       = s"val $settingsName = Array.ofDim[Array[Int]]($settingsCountName)"
    val tableDef          = s"val $tableName = Array.ofDim[Double]($settingsCountName)"
    val dims              = nodes.map(_.domName + ".length").mkString("Array(", " , ", ")")
    val dimsName          = s"_dims_$factorIndex"
    val dimsDef           = s"val $dimsName = $dims"
    val settingIndexName  = s"_settingIndex_$factorIndex"
    val factorName        = s"_factor_$factorIndex"
  }

  def replace(tree: Tree, modification: ModifiedSourceText) = {
    tree match {
      case ApplyArgmax(_, _, dom, pred, obj, _) =>

        //normalize the domain for pattern matching
        val normalizedDom = betaReduce(replaceMethods(inlineVals(dom)))

        //normalize the objective for matching
        val normalizedObj = simplifyBlocks(betaReduce(replaceMethods(inlineVals(obj))))

        //the group of nodes
        val builder = new MPGraphBuilder(normalizedDom, normalizedObj)

        //imports and graph initializer
        val init = Seq(
          "import scalapplcodefest.MPGraph._",
          "import scalapplcodefest._",
          s"val $mpGraphName = new MPGraph()"
        )

        //run inference
        val inference = Seq(
          s"$mpGraphName.build()",
          s"MaxProduct($mpGraphName,4)"
        )

        val modificationSeq = block(init ++ builder.setupStatements ++ inference :+ builder.rootNodeGroup.argmaxValue)

        modification.replace(tree.pos.start, tree.pos.end, normalize(modificationSeq))

        true
      case _ => false

    }
    false
  }


  class MPGraphBuilder(dom: Tree, obj: Tree) {

    val rootNodeName   = "_nodes"
    val rootFactorName = "_factors"

    //get the variable/parameter of the objective
    val Function(List(objVar), objTerm) = obj

    //the root node group, provides access to all node definitions
    val rootNodeGroup = createGroup(dom, "", rootNodeName)

    //the root factor group
    val rootFactorGroup = createFactorGroup("", objTerm)

    val setupStatements =
      rootNodeGroup.allGroups.flatMap(_.init).distinct ++
      rootFactorGroup.allGroups.flatMap(_.init).distinct ++
      Seq(
        s"val $rootNodeName = ${rootNodeGroup.nodesConstructor}",
        s"val $rootFactorName = ${rootFactorGroup.constructor}")


    //a matcher for the objective variable that returns the root node group identifier
    def objVarMatch(tree: Tree) = tree match {
      case Ident(objVar.name) => Some(Ident(rootNodeName))
      case _ => None
    }

    //we use identifiers to match
    trait SpecialIdent extends RefTree {
      def dom: Tree
      def qualifier = EmptyTree
      override def toString() = name.toString
      def defStatement = s"val $name = $dom.toArray"
    }

    case class DomainArrayIdent(dom: Tree) extends SpecialIdent {
      def name = newTermName(treeToName(dom) + "_array")
    }

    object ValueAt {
      def apply(dom: Tree, index: Tree) = Apply(Select(DomainArrayIdent(dom), "apply"), List(index))
    }

    case class DomainIndexIdent(dom: Tree) extends SpecialIdent {
      def name = newTermName(treeToName(dom) + "_index")
    }

    trait NodeGroup {
      def prefix: String
      def nodeGroupType: String
      def nodesConstructor: String
      def argmaxValue: String
      def init: Seq[String]
      def allGroups: List[NodeGroup]

      val indexName  = s"${prefix}_i"
      val resultName = s"${prefix}_result"

      def nodeInfos(tree: Tree, selector: Tree => Option[Tree]): List[NodeInfo]

    }

    case class NodeInfo(selector: Tree, domain: Tree, condition: Option[Tree] = None) {
      val prefix    = selector.toString().replaceAll("\\.", "_")
      val indexName = prefix + "_index"
      val valueName = prefix + "_value"
      val domName   = DomainArrayIdent(domain).toString()
    }

    def createGroup(set: Tree, parentPrefix: String, parentSelector: String): NodeGroup = set match {
      case CaseClassDomain(constructor, dataFields, sets) =>
        CaseClassGroup(parentPrefix, parentSelector, constructor, dataFields, sets)
      case other =>
        AtomNodeGroup(parentSelector, parentPrefix, set)
    }

    case class CaseClassGroup(parentPrefix: String, parentSelector: String,
                              constructor: Tree, fields: List[ValDef], sets: List[Tree]) extends NodeGroup {

      val prefix           = parentPrefix + "_case"
      val subGroups        = (fields zip sets).map(p => createGroup(p._2, prefix + "_" + p._1.name, parentSelector + "." + p._1.name))
      val nodeGroupType    = s"${prefix}_${constructor}Nodes"
      val nodesConstructor = s"$nodeGroupType(${subGroups.map(_.nodesConstructor).mkString(",")})"
      val argmaxValue      = s"$constructor(${subGroups.map(_.argmaxValue).mkString(",")})"
      val nodesClassDef    = {
        val nodesFields = for ((f, g) <- fields zip subGroups) yield s"${f.name}:${g.nodeGroupType}"
        s"case class $nodeGroupType(${nodesFields.mkString(", ")})"
      }
      val init             = Seq(nodesClassDef)
      def allGroups = this :: subGroups.flatMap(_.allGroups)

      def nodeInfos(tree: Tree, selector: Tree => Option[Tree]) = {
        def mySelector(field: ValDef)(tree: Tree) = tree match {
          case Select(q, name) if name == field.name => for (n <- selector(q)) yield Select(n, name)
          case _ => None
        }
        val result = for ((g, f) <- subGroups zip fields) yield g.nodeInfos(tree, mySelector(f))
        result.flatMap(identity)
      }
    }

    case class AtomNodeGroup(parentSelector: String, parentPrefix: String, domain: Tree) extends NodeGroup {

      self =>

      val prefix           = parentPrefix + "_atom"
      val domainId         = DomainArrayIdent(domain)
      val domainName       = domainId.name.toString
      val nodeGroupType    = "Node"
      val nodesConstructor = s"$mpGraphName.addNode($domainName.length)"
      val nodeName         = parentSelector
      val domainDef        = domainId.defStatement
      val init             = Seq(domainDef)
      val argmaxValue      = block(Seq(
        s"val $indexName = MoreArrayOps.maxIndex($nodeName.b)",
        s"$domainName($indexName)"
      ))
      def allGroups = List(this)

      def nodeInfos(tree: Tree, selector: Tree => Option[Tree]) = {
        var result: List[NodeInfo] = Nil
        val traverser = new Traverser {
          override def traverse(tree: Tree) = {
            for (n <- selector(tree)) result ::= NodeInfo(n, domain)
            super.traverse(tree)
          }
        }
        traverser traverse tree
        result
      }


    }

    case class FunArgGroup(parentPrefix: String, parentSelector: String, keyTree: Tree, valueTree: Tree) extends NodeGroup {

      val prefix           = s"${parentPrefix}_arg"
      val selector         = s"$parentSelector($indexName)"
      val subGroup         = createGroup(valueTree, prefix, selector)
      val nodeGroupType    = s"Array[${subGroup.nodeGroupType}]"
      val domainId         = s"${prefix}_dom"
      val domainDef        = s"val $domainId = $keyTree"
      val init             = Seq(domainDef)
      val argmaxValue      = s"$domainId.indices.view.map($indexName => $domainId($indexName) -> ${subGroup.argmaxValue}).toMap"
      val nodesConstructor = block(Seq(
        s"val $resultName = Array.ofDim[${subGroup.nodeGroupType}]($domainId.length)",
        whileLoopRange(indexName, s"$domainId.length", "0") {s"$resultName($indexName) = ${subGroup.nodesConstructor}"},
        resultName
      ))

      def allGroups = this :: subGroup.allGroups

      def nodeInfos(tree: Tree, selector: Tree => Option[Tree]) = {
        def mySelector(tree: Tree) = tree match {
          case Apply(fun, args) => for (s <- selector(fun)) yield {
            val domainApply = Select(DomainIndexIdent(keyTree), "apply")
            val indexOfArg = Apply(domainApply, args)
            Apply(s, List(indexOfArg))
          }
          case _ => None
        }
        subGroup.nodeInfos(tree, mySelector)
      }

    }

    case class NodeDef(prefix: String, selector: String, atoms: AtomNodeGroup, condition: Option[Tree] = None) {
      val indexName = prefix + "_index"
      val valueName = prefix + "_value"

    }

    def createFactorGroup(parentPrefix: String, tree: Tree): FactorGroup = tree match {
      case FlatDoubleSum(args) => PropositionalDoubleSum(parentPrefix, args)
      case ApplySum(_, _, domain, pred, Function(List(arg), term), _) => FirstOrderDoubleSum(parentPrefix, arg.symbol, domain, term)
      case other => SingleFactor(parentPrefix, tree)
    }

    trait FactorGroup {
      def factorGroupType: String
      def constructor: String
      def prefix: String
      def init: Seq[String]
      def allGroups: List[FactorGroup]

    }

    case class SingleFactor(parentPrefix: String, potential: Tree) extends FactorGroup {

      val factorGroupType   = "Factor"
      val prefix            = parentPrefix + "_factor"
      val nodes             = rootNodeGroup.nodeInfos(potential, objVarMatch)
      val tree2node         = nodes.map(n => n.selector -> n).toMap
      val hidden            = nodes.filter(_.condition.isEmpty)
      val tableName         = prefix + "_table"
      val settingsName      = prefix + "_settings"
      val settingsIndexName = prefix + "_settingsIndex"
      val settingsCountName = prefix + "_settingsCount"
      val dimsName          = prefix + "_dims"
      val constructor       = block(Seq(
        s"val $settingsCountName = ${("1" +: hidden.map(n => s"${n.domName}.length")).mkString(" * ")}",
        s"val $tableName = Array.ofDim[Double]($settingsCountName)",
        s"val $settingsName = Array.ofDim[Array[Int]]($settingsCountName)",
        s"val $dimsName = Array(${hidden.map(n => s"${n.domName}.length").mkString(",")})",
        s"var $settingsIndexName = 0",
        generateSettings(hidden, this),
        s"$mpGraphName.addTableFactor($tableName, $settingsName, $dimsName)"
      ))
      def init = Seq.empty
      def allGroups = List(this)
    }

    case class PropositionalDoubleSum(parentPrefix: String, args: List[Tree]) extends FactorGroup {
      val prefix          = parentPrefix + "_psum"
      val subgroups       = args map (createFactorGroup(prefix, _))
      val factorGroupType = s"Tuple${args.size}[%s]".format(subgroups.map(_.factorGroupType).mkString(","))
      def constructor = block(
        subgroups.zipWithIndex.map(g => s"val _factor_${g._2} = ${g._1.constructor}") :+
        subgroups.indices.map(i => s"_factor_$i").mkString("(", ", ", ")")
      )
      def init = Seq.empty
      def allGroups = this :: subgroups.flatMap(_.allGroups)
    }

    case class FirstOrderDoubleSum(parentPrefix: String, variable: Symbol, domain: Tree, term: Tree) extends FactorGroup {

      //define integer index
      val prefix     = parentPrefix + "_sum"
      val indexName  = prefix + "_index"
      val valueName  = prefix + "_value"
      val resultName = prefix + "_result"
      val indexTerm  = Ident(indexName)
      val domId      = DomainArrayIdent(domain)
      val domName    = domId.toString()

      //replace occurences of variable in term by domain(index)
      val substituted     = transform(term, {case Ident(v) if v == variable => ValueAt(domain, indexTerm)})
      //create subgroup from substituted term
      val subGroup        = createFactorGroup(prefix, substituted)
      val factorGroupType = s"Array[${subGroup.factorGroupType}}]"

      //create array of factor groups depending on subgroup type
      val constructor = block(Seq(
        s"val $resultName = Array.ofDim[${subGroup.factorGroupType}]($domName.length)",
        whileLoopRange(indexName, s"$domName.length", "0") {s"$resultName($indexName) = ${subGroup.constructor}"},
        resultName
      ))
      def init = Seq(domId.defStatement)
      def allGroups = this :: subGroup.allGroups
    }

    def generateSettings(nodesLef: List[NodeInfo], factor: SingleFactor): String = {
      import factor._
      nodesLef match {
        case Nil =>
          val settingDef = s"val _setting = ${hidden.map(n => s"${n.indexName}").mkString("Array(", ",", ")")}"
          val updateSetting = s"$settingsName($settingsIndexName) = _setting"
          val substituted = transform(potential, {
            case tree => tree2node.get(tree) match {
              case Some(node) => node.condition.getOrElse(Ident(node.valueName))
              case _ => tree
            }
          })
          val scoreDef = s"$tableName($settingsIndexName) = $substituted"
          val settingInc = s"$settingsIndexName += 1"
          block(Seq(settingDef, updateSetting, scoreDef, settingInc))
        case node :: tail =>
          val iName = node.indexName
          val vName = node.valueName
          val dName = DomainArrayIdent(node.domain).toString()
          whileLoopRange(iName, dName + ".length") {s"val $vName = $dName($iName);\n${generateSettings(tail, factor)}"}
      }
    }

  }


}
