package scalapplcodefest.sbt

/**
 * @author Sebastian Riedel
 */
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
            //todo: this only works for case classes with two fields
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
    case ApplyArgmax(_, _, dom, pred, obj, num) => newDomainAndPredicate(dom,pred) match {
      case Some((newDomain,newPredicate)) => Some(s"""argmax ($newDomain)($newPredicate)($obj)($num)""")
      case _ => None
    }
    case _ => None
  }


  def replace(tree: Tree, modification: ModifiedSourceText) = {
    replacementText(tree) match {
      case Some(text) => modification.replace(tree.pos.start, tree.pos.end, env.normalize(text)); true
      case None => false
    }
  }

}

object ConditionReplacer {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/Iris.scala",
      replacers = List(new ConditionReplacer(_)))
  }
}
