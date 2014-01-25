package scalapplcodefest.sbt


/**
 * @author Sebastian Riedel
 */
class MPGraphReplacer(val env: GeneratorEnvironment) extends CodeStringReplacer with WolfePatterns {

  import env._
  import global._

  //the mpGraph variable name
  val mpGraphName = "_mpGraph"


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
            Ident(node.valueName)
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


  object EqualityConditions {
    def unapply(tree: Tree) = tree match {
      case Function(List(dataArgDef), FieldEquality(dataArgUse, fieldName, conditionValue)) =>
        Some(List(fieldName -> conditionValue))
      case _ => None
    }
  }

  case class NodeDefField(field: ValDef, dom: Tree, nodeName: String, domName: String) {
    val fieldName = field.name.encoded
    val indexName = s"_index_$fieldName"
    val valueName = s"_value_$fieldName"
    val domDef    = s"val $domName = $dom.toArray"
    val nodeDef   = s"val $nodeName = $mpGraphName.addNode($domName.size)"
  }

  case class FactorDef(factorIndex: Int, potential: Tree, nodes: List[NodeDefField]) {
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

        //get equality conditions
        val EqualityConditions(conditions) = pred

        //normalize the domain for pattern matching
        val normalizedDom = betaReduce(replaceMethods(inlineVals(dom)))

        //get the constructor for creating the result
        val CaseClassDomain(constructor, _, _) = normalizedDom

        //find free variables (todo: exclude observed variables)
        val nodeDefs = normalizedDom match {
          case CaseClassDomain(_, dataFields, sets) =>
            for ((field, set) <- dataFields zip sets) yield {
              val domName = s"_dom_${field.name.encoded}"
              val nodeName = s"_node_${field.name.encoded}"
              println(constructor)
              NodeDefField(field, set, nodeName, domName)
            }
          case _ => Nil
        }
        val fieldName2nodeDef = nodeDefs.map(n => n.field.name -> n).toMap

        //normalize the objective for matching
        val normalizedObj = betaReduce(replaceMethods(obj))

        //get data argument of objective
        val Function(List(objArg), _) = normalizedObj

        //get factor definitions
        val factorDefs = normalizedObj match {
          case Function(_, Apply(log, List(Block(_, FlatDoubleProduct(args))))) =>
            for ((arg, index) <- args.zipWithIndex) yield {
              //find variables in arg
              val selects = arg.collect({case s@Select(i@Ident(_), _) if i.symbol == objArg.symbol => s})
              //get the node definitions for these variables
              val nodes = selects.map(s => fieldName2nodeDef(s.name))
              println(selects)
              println(nodes)
              //build the potential table
              val factorDef = FactorDef(index, arg, nodes)
              factorDef
            }
          case _ => Nil
        }

        //the generated string corresponding to the node and domain definitions
        val nodeDefStrings = for (nodeDef <- nodeDefs) yield Seq(nodeDef.domDef, nodeDef.nodeDef).mkString(";\n")

        //the generated string corresponding to the factor definitions
        val factorDefStrings = for (factorDef <- factorDefs) yield {
          import factorDef._

          val setupTables = s"{ var $settingIndexName = 0; ${settingsLoop(nodes, factorDef, objArg.symbol, fieldName2nodeDef)} }"
          val createFactor = s"val $factorName = $mpGraphName.addTableFactor($tableName, $settingsName, $dimsName)"
          val addEdges = nodeDefs.zipWithIndex.map({
            case (n, i) => s"$mpGraphName.addEdge($factorName,${n.nodeName},$i)"
          }).mkString(";\n")
          Seq(dimsDef, settingsCountDef, tableDef, settingsDef, setupTables, createFactor, addEdges).mkString(";\n")
        }

        //imports and graph initializer
        val init = Seq(
          "import scalapplcodefest._",
          s"val $mpGraphName = new MPGraph()"
        )

        //run inference
        val inference = Seq(
          s"MaxProduct($mpGraphName,4)"
        )

        //convert inference result into data object
        val result = {
          val setWinners = (for (node <- nodeDefs) yield {
            import node._
            Seq(s"val $indexName = MoreArrayOps.maxIndex(${node.nodeName}.b)", s"val $valueName = $domName($indexName)")
          }).flatMap(identity)
          val create = s"$constructor(${nodeDefs.map(_.valueName).mkString(",")})"
          s"{${(setWinners :+ create).mkString(";\n")}}"
        }

        val modificationSeq = init ++ nodeDefStrings ++ factorDefStrings ++ inference :+ result

        modification.replace(tree.pos.start, tree.pos.end, normalize(modificationSeq.mkString("{", ";\n", "}")))
        true
      case _ => false

    }
    false
  }
}
