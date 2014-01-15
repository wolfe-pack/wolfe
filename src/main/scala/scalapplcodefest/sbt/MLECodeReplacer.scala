package scalapplcodefest.sbt


/**
 * @author Sebastian Riedel
 */
class MLECodeReplacer(val env: GeneratorEnvironment) extends CodeStringReplacer with WolfePatterns {
  def replace(tree: env.global.Tree, modification: ModifiedSourceText): Boolean = {

    val reduced = env.betaReduce(tree)

    reduced match {
      case ApplyArgmin2(_, vectors, _, LogLikelihood2(data, domain, f, w), _)
        if f.symbol.hasAnnotation(MarkerOneHot) =>

        val featName = f.symbol.name.toString
        val sum = s"""sum2 ($data) (_ => true) { y_i => $featName(y_i) } mapValues(w => math.log(w / $data.size)) """
        modification.replace(tree.pos.start, tree.pos.end, sum)
        true
      case _ =>
        false
    }
  }

}

object MLECodeReplacer {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/CoinTossing.scala",
      replacers = List(new MLECodeReplacer(_)))
  }
}

class ConditionReplacer(val env: GeneratorEnvironment) extends CodeStringReplacer with WolfePatterns {

  import env.global._


  def replace(tree: Tree, modification: ModifiedSourceText) = env.betaReduce(tree) match {
    case ApplyArgmax2(_, dom, pred, obj, num) =>
      val data = env.inlineVals(dom)
      data match {
        case CaseClassDomain(constructor, dataFields, sets) =>
          pred match {
            case Function(List(dataArgDef), FieldEquality(dataArgUse, fieldName, conditionValue)) =>
              val indexOfField = dataFields.indexWhere(_.name == fieldName)
              val crossProductArgs = for ((set, index) <- sets.zipWithIndex) yield
                if (index == indexOfField) s"Set($conditionValue)" else set.toString()
              val replacement = s"""argmax2 (all(unwrap2($constructor))(c(${crossProductArgs.mkString(",")}))) (_ => true)($obj)"""
              //find out argument index of fieldName
              modification.replace(tree.pos.start, tree.pos.end, replacement)
              true
            case _ => false
          }
        case _ => false
      }
      false
    case _ => false

  }

}

object ConditionReplacer {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/Conditioning.scala",
      replacers = List(new ConditionReplacer(_)))
  }
}
