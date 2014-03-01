package scalapplcodefest.sbt

import scalapplcodefest.MPGraph
import scala.reflect._


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

  def treeToName(tree: Tree, postfix: String = "") = {
    val string = tree.toString()
    //"`" + string + "`"
    string.replaceAll("[\\.\\[\\]\\(\\)\\,]", "_").replaceAll("\t\f\n\r", "")
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
          "import scala.reflect._",
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
      (rootNodeGroup.allGroups.flatMap(_.init) ++ rootFactorGroup.allGroups.flatMap(_.init)).distinct ++
      Seq(
        s"val $rootNodeName = ${rootNodeGroup.nodesConstructor}",
        s"val $rootFactorName = ${rootFactorGroup.constructor}")


    //a matcher for the objective variable that returns the root node group identifier
    def objVarMatch(tree: Tree) = tree match {
      case Ident(objVar.name) => Some(Ident(rootNodeName))
      case _ => None
    }

    def rootMatcher(tree: Tree) = tree match {
      case Ident(objVar.name) => Some(Structure(Ident(rootNodeName), rootNodeGroup))
      case _ => None
    }

    def allMatcher = structureMatcher(rootNodeGroup, rootMatcher, rootMatcher)


    //we use identifiers to match
    trait SpecialIdent extends RefTree {
      def dom: Tree
      def qualifier = EmptyTree
      override def toString() = name.toString
      def ident = Ident(name)
    }

    case class DomainArrayIdent(dom: Tree) extends SpecialIdent {
      def defStatement = s"val $name = $dom.toArray"
      def name = newTermName(treeToName(dom) + "_array")
    }

    object ValueAt {
      def apply(dom: Tree, index: Tree) = Apply(Select(DomainArrayIdent(dom).ident, "apply"), List(index))
    }

    case class DomainIndexIdent(dom: Tree) extends SpecialIdent {
      def name = newTermName(treeToName(dom) + "_index")
      def defStatement = s"val $name = $dom.zipWithIndex.toMap"
    }

    trait StructureType {
      def prefix: String
      def nodeGroupType: String
      def nodesConstructor: String
      def argmaxValue: String
      def init: Seq[String]
      def allGroups: List[StructureType]

      def indexName = s"${prefix}_i"
      def resultName = s"${prefix}_result"

      def nodeInfos(tree: Tree, selector: Tree => Option[Tree]): List[NodeInfo]


    }

    def structureMatcher(typ: StructureType, parent: Tree => Option[Structure],
                         all: Tree => Option[Structure]): Tree => Option[Structure] = {
      typ match {
        case a: AtomicType => all
        case c: CaseClassType =>
          def matcher(field: ValDef)(tree: Tree) = tree match {
            case Select(q, name) if name == field.name => for (n <- parent(q)) yield
              Structure(Select(n.selector, name), c.fieldToGroup(name))
            case _ => None
          }
          //first try all field matchers
          val fieldMatchers = c.fields.map(f => structureMatcher(c.fieldToGroup(f.name), matcher(f), matcher(f)))
          def firstMatcher(matchers: List[Tree => Option[Structure]]): Tree => Option[Structure] = matchers match {
            case Nil => all
            case head :: tail => (t: Tree) => head(t).orElse(firstMatcher(tail)(t))
          }
          firstMatcher(fieldMatchers)
        case f: FunType =>
          def matcher(tree: Tree) = tree match {
            case Apply(Select(fun, apply), args) if apply.encoded == "apply" => for (s <- parent(fun)) yield {
              val domainApply = Select(f.domainIndexId.ident, "apply")
              val indexOfArg = Apply(domainApply, args)
              Structure(Apply(s.selector, List(indexOfArg)), f.valueType)
            }
            case _ => None
          }
          structureMatcher(f.valueType, matcher, t => matcher(t).orElse(all(t)))
      }
    }

    //finds all structures that are referenced in the tree
    def structures(typ: StructureType,
                   tree: Tree,
                   parent: Tree => Option[Structure] = rootMatcher,
                   all: Tree => Option[Structure] = rootMatcher): List[Structure] = {
      val matcher = structureMatcher(typ, parent, all)
      var result: List[Structure] = Nil
      val traverser = new Traverser {
        override def traverse(tree: Tree) = {
          matcher(tree) match {
            case Some(structure) =>
              result ::= structure
            case _ =>
              super.traverse(tree)
          }
        }
      }
      traverser traverse tree
      result
    }

    def injectStructure(tree:Tree, matcher:Tree => Option[Structure] = allMatcher):Tree = {
      val transformer = new Transformer{
        override def transform(tree: Tree) = {
          matcher(tree) match {
            case Some(structure) =>
              val structureValue = valueOfStructure(structure)
              structureValue.tree
            case _ => super.transform(tree)
          }
        }
      }
      transformer transform tree
    }

    def expr[T: WeakTypeTag](tree: Tree) = Expr[T](rootMirror, FixedMirrorTreeCreator(rootMirror, tree))

    def valueOfStructure(structure: Structure): Expr[Any] = structure.typ match {
      case a: AtomicType =>
        val node = expr[MPGraph.Node](structure.selector)
        reify(a.domainExpr.splice(node.splice.value))
      case c: CaseClassType =>
        val args = c.substructures(structure).map(valueOfStructure)
        val apply = Apply(Select(Ident(c.nodeGroupType), "apply"), args.map(_.tree))
        expr[Any](apply)
      case f: FunType =>
        val iSymbol = f.domainExpr.tree.symbol.newValue(newTermName("i")).setInfo(definitions.IntTpe)
        val i = expr[Int](Ident(iSymbol))
        val sub = valueOfStructure(Structure(Apply(Select(structure.selector, "apply"), List(Ident(iSymbol))), f.valueType))
        val mapping = Function(List(ValDef(iSymbol, EmptyTree)), reify(f.domainExpr.splice(i.splice) -> sub.splice).tree)
        reify(Range(0,f.domainExpr.splice.length).map( expr[Int => (Any,Any)](mapping).splice))
    }

    def nodesOfStructure(structure: Structure): Expr[Iterator[MPGraph.Node]] = structure.typ match {
      case a: AtomicType =>
        val node = expr[MPGraph.Node](structure.selector)
        val iterator = reify(Iterator(node.splice))
        iterator
      case c: CaseClassType =>
        val trees = (for (f <- c.fields) yield {
          val selector = Select(structure.selector, f.name)
          val sub = nodesOfStructure(Structure(selector, c.fieldToGroup(f.name)))
          sub
        }).toArray
        def reduce(t1: Expr[Iterator[MPGraph.Node]], t2: Expr[Iterator[MPGraph.Node]]) = reify(t1.splice ++ t2.splice)
        val reduced = trees.reduce(reduce)
        reduced
      case f: FunType =>
        def sub(index: Expr[Int]) = {
          val selector = reify(expr[Array[Any]](structure.selector).splice(index.splice))
          nodesOfStructure(Structure(selector.tree, f.valueType))
        }
        val arrayDom = reify(expr[Array[Any]](f.domainId.ident).splice.indices)
        val i = arrayDom.tree.symbol.newValue(newTermName("i")).setInfo(definitions.IntTpe)
        val mapping = expr[Int => Iterator[MPGraph.Node]](Function(List(ValDef(i, EmptyTree)), sub(expr[Int](Ident(i))).tree))
        val result = reify(arrayDom.splice.iterator.flatMap(i => mapping.splice(i)))
        result
    }

    case class Structure(selector: Tree, typ: StructureType)


    case class NodeInfo(nodeSelector: Tree, matcher: Tree, domain: Tree, condition: Option[Tree] = None) {
      val prefix    = treeToName(nodeSelector)
      //.toString().replaceAll("\\.", "_")
      val indexName = prefix + "_index"
      val valueName = prefix + "_value"
      val domName   = DomainArrayIdent(domain).toString()
    }

    def createGroup(set: Tree, parentPrefix: String, parentSelector: String): StructureType = set match {
      case CaseClassDomain(constructor, dataFields, sets) =>
        CaseClassType(parentPrefix, parentSelector, constructor, dataFields, sets)
      case Apply(TypeApply(pred, _), List(keyTree)) if pred.symbol.name.toString() == "Pred" =>
        FunType(parentPrefix, parentSelector, keyTree, Select(Select(Ident(scalapplcodefest), "Wolfe"), "bools")) //todo: do this properly
      case other =>
        AtomicType(parentSelector, parentPrefix, set)
    }

    case class CaseClassType(parentPrefix: String, parentSelector: String,
                             constructor: Tree, fields: List[ValDef], sets: List[Tree]) extends StructureType {

      def substructures(structure: Structure) = fields.map(f => Structure(Select(structure.selector, f.name), fieldToGroup(f.name)))

      val prefix       = parentPrefix + "_case"
      val subGroups    = (fields zip sets).map(p => createGroup(p._2, prefix + "_" + p._1.name, parentSelector + "." + p._1.name))
      val fieldToGroup = (fields.map(_.name) zip subGroups).toMap

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

    case class AtomicType(parentSelector: String, parentPrefix: String, domain: Tree) extends StructureType {

      self =>

      val prefix           = parentPrefix + "_atom"
      val domainId         = DomainArrayIdent(domain)
      val domainExpr       = expr[Array[Any]](domainId.ident)
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
            for (n <- selector(tree)) result ::= NodeInfo(n, tree, domain)
            super.traverse(tree)
          }
        }
        traverser traverse tree
        result
      }


    }

    case class FunType(parentPrefix: String, parentSelector: String, keyTree: Tree, valueTree: Tree) extends StructureType {

      val prefix           = s"${parentPrefix}_arg"
      val selector         = s"$parentSelector($indexName)"
      val valueType        = createGroup(valueTree, prefix, selector)
      val nodeGroupType    = s"Array[${valueType.nodeGroupType}]"
      val domainId         = DomainArrayIdent(keyTree)
      val domainExpr       = expr[Array[Any]](domainId.ident)
      val domainIndexId    = DomainIndexIdent(keyTree)
      val domainDef        = domainId.defStatement
      val init             = Seq(domainDef, domainIndexId.defStatement)
      val argmaxValue      = s"$domainId.indices.view.map($indexName => $domainId($indexName) -> ${valueType.argmaxValue}).toMap"
      val nodesConstructor = block(Seq(
        s"val $resultName = Array.ofDim[${valueType.nodeGroupType}]($domainId.length)",
        whileLoopRange(indexName, s"$domainId.length", "0") {s"$resultName($indexName) = ${valueType.nodesConstructor}"},
        resultName
      ))

      def allGroups = this :: valueType.allGroups

      def nodeInfos(tree: Tree, selector: Tree => Option[Tree]) = {
        def mySelector(tree: Tree) = tree match {
          case Apply(Select(fun, apply), args) if apply.encoded == "apply" => for (s <- selector(fun)) yield {
            val domainApply = Select(domainIndexId.ident, "apply")
            val indexOfArg = Apply(domainApply, args)
            Apply(s, List(indexOfArg))
          }
          case _ => None
        }
        valueType.nodeInfos(tree, mySelector)
      }

    }


    def createFactorGroup(parentPrefix: String, tree: Tree): FactorType = tree match {
      case Block(_, result) => createFactorGroup(parentPrefix, result)
      case FlatDoubleSum(args) => PropositionalDoubleSum(parentPrefix, args)
      case ApplySum(_, _, domain, pred, Function(List(arg), term), _) => FirstOrderDoubleSum(parentPrefix, arg.symbol, domain, term)
      case other => SingleFactor(parentPrefix, tree)
    }

    trait FactorType {
      def factorGroupType: String
      def constructor: String
      def prefix: String
      def init: Seq[String]
      def allGroups: List[FactorType]

    }

    case class SingleFactor(parentPrefix: String, potential: Tree) extends FactorType {

      val structs      = structures(rootNodeGroup, potential)
      val emptyNode    = reify[Iterator[MPGraph.Node]](Iterator.empty)
      val nodeIterator = structs match {
        case Nil => emptyNode
        case list => list.map(nodesOfStructure).reduce[Expr[Iterator[MPGraph.Node]]]({case (a1, a2) => reify(a1.splice ++ a2.splice)})
      }

      //        structs.map(nodesInStructure).foldLeft[Expr[Iterator[MPGraph.Node]]](emptyNode)({case (a1, a2) => reify(a1.splice ++ a2.splice)})
      val nodeArray     = reify(nodeIterator.splice.toArray(classTag[MPGraph.Node]))
      val setting       = reify(nodeIterator.splice.map(_.setting).toArray(classTag[Int]))
      val settingsCount = reify(nodeIterator.splice.map(_.dim).product)

      def processSetting(table: Expr[Array[Double]], settings: Expr[Array[Array[Int]]], settingIndex: Expr[Int]) = reify({
        settings.splice(settingIndex.splice) = nodeIterator.splice.map(_.setting).toArray(classTag[Int])
        table.splice(settingIndex.splice) = 0.0
      })

      val injectedPot = expr[Double](injectStructure(potential))

      val constructorBlock = reify({
        def _f_nodeIterator = nodeIterator.splice
        val _f_nodes = _f_nodeIterator.toArray(classTag[MPGraph.Node])
        val _f_settingCount = _f_nodeIterator.map(_.dim).product
        val _f_setting = _f_nodeIterator.map(_.setting).toArray(classTag[Int])
        val _f_table = Array.ofDim[Double](_f_settingCount)
        val _f_settings = Array.ofDim[Array[Int]](_f_settingCount)
        val _f_value = valueOfStructure(structs.head).splice
        val _f_injected = injectedPot.splice
        println(_f_settingCount)
      })

      val factorGroupType   = "Factor"
      val prefix            = parentPrefix + "_factor"
      val nodes             = rootNodeGroup.nodeInfos(potential, objVarMatch)
      val treeString2node   = nodes.map(n => n.matcher.toString() -> n).toMap
      val hidden            = nodes.filter(_.condition.isEmpty)
      val tableName         = prefix + "_table"
      val settingsName      = prefix + "_settings"
      val settingsIndexName = prefix + "_settingsIndex"
      val settingsCountName = prefix + "_settingsCount"
      val dimsName          = prefix + "_dims"
      val constructor       = block(Seq(
        //todo: get domain from each node. This may have been set through conditions
        asCompactString(constructorBlock.tree),
        s"val $settingsCountName = ${("1" +: hidden.map(n => s"${n.domName}.length")).mkString(" * ")}",
        s"val $tableName = Array.ofDim[Double]($settingsCountName)",
        s"val $settingsName = Array.ofDim[Array[Int]]($settingsCountName)",
        s"val $dimsName = Array(${hidden.map(n => s"${n.domName}.length").mkString(",")})",
        s"var $settingsIndexName = 0",
        generateSettings(hidden, this),
        s"val _tmp = $mpGraphName.addTableFactor($tableName, $settingsName, $dimsName)",
        block(hidden.zipWithIndex.map({case (n, i) => s"$mpGraphName.addEdge(_tmp,${n.nodeSelector},$i)"})),
        "_tmp"
      ))
      def init = Seq.empty
      def allGroups = List(this)
    }

    case class PropositionalDoubleSum(parentPrefix: String, args: List[Tree]) extends FactorType {
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

    case class FirstOrderDoubleSum(parentPrefix: String, variable: Symbol, domain: Tree, term: Tree) extends FactorType {

      //define integer index
      val prefix     = parentPrefix + "_sum"
      val indexName  = prefix + "_index"
      val valueName  = prefix + "_value"
      val resultName = prefix + "_result"
      val indexTerm  = Ident(indexName)
      val domId      = DomainArrayIdent(domain)
      val domName    = domId.toString()

      //replace occurences of variable in term by domain(index)
      val substituted = transform(term, {
        case i@Ident(_) if i.symbol == variable =>
          ValueAt(domain, indexTerm)
      })

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
            case Import(_, _) => EmptyTree //todo: this should only remove imports for the objective arg
            case tree => {
              treeString2node.get(tree.toString()) match {
                case Some(node) =>
                  node.condition.getOrElse(Ident(node.valueName))
                case _ => tree
              }
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
