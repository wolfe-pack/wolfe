package ml.wolfe.term

/**
 * @author riedel
 */
object NTP {

  import ml.wolfe.Language._

  @termdef case class Output(success:Tensor, result:Tensor)
  @termdef case class Atom(rel:Tensor, arg1:Tensor, arg2:Tensor)

  type ProofUnit = Term[Tensor] => Term[Output]

  def embed(embeddings:Term[Tensor])(atom:Term[Atom]) = {
    val e_rel = embeddings * atom.rel
    val e_arg1 = embeddings * atom.arg1
    val e_arg2 = embeddings * atom.arg2
    concat(e_rel, e_arg1, e_arg2)
  }

  def unify(candidate:Term[Tensor])(query:Term[Tensor]) = {
    Output.Term(sigmoid(candidate.t * query), candidate)
  }

  def disjunction(units:Seq[ProofUnit])(query:Term[Tensor]) = {
    units.foldLeft(Output.Term(???,???)) {
      (last,unit) =>
        val output = unit(query)
        val success =  max(last.success,output.success)
        val result = output.result
        Output.Term(success, result)
    }
  }

  def rule1(head:Term[Tensor], translate:Term[Tensor], body1:ProofUnit)(query:Term[Tensor]) = {
    val unifies = sigmoid(head.t * query)
    val translated = translate * query
    val o1 = body1(translated)
    Output.Term(max(o1.success, unifies), o1.result) //should this invert the result using the inverse translation?
  }

  def main(args: Array[String]) {

  }


}
