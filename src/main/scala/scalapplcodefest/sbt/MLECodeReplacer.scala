package scalapplcodefest.sbt


/**
 * @author Sebastian Riedel
 */
class MLECodeReplacer(val env: GeneratorEnvironment) extends CodeStringReplacer with WolfePatterns {
  def replace(tree: env.global.Tree, modification: ModifiedSourceText): Boolean = {

    val replaced = env.replaceMethods(tree)
    val reduced = env.betaReduce(replaced)

    reduced match {
      case ApplyArgmin2(_, _, vectors, _, LogLikelihood2(data, domain, f, w), _)
        if f.symbol.hasAnnotation(MarkerOneHot) =>

        val featName = f.symbol.name.toString
        val sum = s"""sum2 ($data) (_ => true) { y_i => $featName(y_i) } mapValues(w => math.log(w / $data.size)) """
        modification.replace(tree.pos.start, tree.pos.end, sum)
        true
      case ApplyArgmin2(_, _, _, _, _, _) =>
        println(replaced)
        println(reduced)
        false

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

  def newDomainAndPredicate(dom: Tree, pred: Tree): Option[(String, String)] = {
    val data = env.inlineVals(dom)
    data match {
      case CaseClassDomain(constructor, dataFields, sets) =>
        pred match {
          case Function(List(dataArgDef), FieldEquality(dataArgUse, fieldName, conditionValue)) =>
            val indexOfField = dataFields.indexWhere(_.name == fieldName)
            val crossProductArgs = for ((set, index) <- sets.zipWithIndex) yield
              if (index == indexOfField) s"Set($conditionValue)" else set.toString()
            val newDomain = s"""all(unwrap2($constructor))(c(${crossProductArgs.mkString(",")}))"""
            val newPredicate = """_ => true"""
            //find out argument index of fieldName
            Some(newDomain,newPredicate)
          case _ => None
        }
      case _ => None
    }
  }


  def replacementText(tree: Tree): Option[String] = env.betaReduce(tree) match {
    case ApplyArgmax2(_, _, dom, pred, obj, num) => newDomainAndPredicate(dom,pred) match {
      case Some((newDomain,newPredicate)) => Some(s"""argmax2 ($newDomain)($newPredicate)($obj)($num)""")
      case _ => None
    }
    case _ => None
  }


  def replace(tree: Tree, modification: ModifiedSourceText) = {
    replacementText(tree) match {
      case Some(text) => modification.replace(tree.pos.start, tree.pos.end, text); true
      case None => false
    }
  }

}

object ConditionReplacer {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/Conditioning.scala",
      replacers = List(new ConditionReplacer(_)))
  }
}
