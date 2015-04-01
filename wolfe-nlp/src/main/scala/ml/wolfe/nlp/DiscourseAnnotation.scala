package ml.wolfe.nlp

/**
 * Class to represent discourse annotation
 * @param relations sequence of DiscourseRelation elements
 */

case class DiscourseAnnotation(relations: Seq[DiscourseRelation] = Seq.empty)

case class DiscourseRelation(arg1: DiscourseArgument,
                             arg2: DiscourseArgument,
                             connective: DiscourseArgument,
                             id: String,
                             sense: List[String],
                             typ: String)

case class DiscourseArgument(text: String = "",
                             charOffsets: List[CharOffsets] = List.empty,
                             tokens: Seq[(Int, Int)] = Seq.empty)

object DiscourseArgument {
  val empty = DiscourseArgument()
}

object DiscourseAnnotation {
  val empty = DiscourseAnnotation()
}

