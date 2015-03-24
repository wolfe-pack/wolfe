package ml.wolfe.nlp.io


import org.json4s._
import org.json4s.jackson.JsonMethods._

/**
 * @author mbosnjak
 */

// Algebra Word Problems reader
// for "Learning to Automatically Solve Algebra Word Problems" data
// downloadable at: http://groups.csail.mit.edu/rbg/code/wordprobs/questions.json

case class AlgebraProblem(id: Int, question: String, equations: Iterable[String], solutions: Iterable[Double])


class AlgebraProblemReader(filename: String) extends Iterable[AlgebraProblem] {

  def iterator: Iterator[AlgebraProblem] = {
    val reader = new java.io.FileReader(filename)
    val json = parse(reader)
    reader.close()
    val list =
      for {
        JObject(problem) <- json
        JField("iIndex", JInt(iIndex)) <- problem
        JField("sQuestion", JString(sQuestion)) <- problem
        JField("lEquations", JArray(lEquations)) <- problem
        JField("lSolutions", JArray(lSolutions)) <- problem
      } yield {
        val equations = lEquations.map(_.asInstanceOf[JString].s).toIterable
        val solutions = lSolutions.map(_.asInstanceOf[JDouble].num).toIterable
        AlgebraProblem(iIndex.toInt, sQuestion, equations, solutions)
      }
    list.asInstanceOf[List[AlgebraProblem]].toIterator
  }
}

object AlgebraProblemReader {
  def main(args: Array[String]) {
    for (q <- new AlgebraProblemReader(args(0))) {
      println(q) + "\n"
    }
  }
}
