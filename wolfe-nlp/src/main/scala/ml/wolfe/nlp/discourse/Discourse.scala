package ml.wolfe.nlp.discourse

import ml.wolfe.nlp.CharOffsets

/**
 * Created by matko on 3/20/15.
 */
case class DiscourseRelation(arg1: DiscourseArgument,
                             arg2: DiscourseArgument,
                             connective: DiscourseArgument,
                             id: String,
                             sense: List[String],
                             typ: String)

case class DiscourseArgument(text: String = "",
                             charOffsets: List[CharOffsets] = List.empty,
                             tokens: Seq[(Int, Int)] = Seq.empty) {
  def charSpan:CharOffsets = CharOffsets(charOffsets.head.start,charOffsets.last.end)
}

object DiscourseArgument {
  val empty = DiscourseArgument()
}
