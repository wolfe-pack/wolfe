package scalapplcodefest.example

import scalapplcodefest._
import scala.collection.mutable
import scala.util.Random

/**
 * The smokes & cancer MLN.
 *
 * @author Sebastian Riedel
 */
object MarkovLogicExample extends App {

  import Wolfe._

  type Person = Symbol

  case class Data(smokes: Person => Boolean, cancer: Person => Boolean, friends: Map[(Person, Person), Boolean])

  val persons = Set('Anna, 'Bob)
  val bools = Set(true, false)

  def mln(data: Data, weights: Vector) = {

    import data._

    def f1 = sum(persons) {p => ft('smokingIsBad, smokes(p) -> cancer(p))}

    def f2 = sum(c(persons, persons)) {
      case (p1, p2) => ft('peerPressure, friends(p1, p2) -> (smokes(p1) <-> smokes(p2)))
    }
    (f1 + f2) dot weights
  }

  val weights = Vector('smokingIsBad -> 2.0, 'peerPressure -> 0.0)

  def hidden = for (smokes <- maps(persons, bools);
                    cancer <- maps(persons, bools);
                    friends <- maps(c(persons, persons), bools))
  yield Data(smokes, cancer, friends)

  def observed(h: Data) = h.smokes('Anna) && h.cancer('Anna) && (h.friends only (('Anna, 'Bob)))

  val prediction3 = argmax(hidden filter observed) {y => mln(y, weights)}
  println(s"smokes: ${prediction3.smokes}, cancer: ${prediction3.cancer}")

  val smokeCounts = new mutable.HashMap[Person, Int]
  val cancerCounts = new mutable.HashMap[Person, Int]
  for (i <- 0 until 1000) {
    implicit val random = new Random()
    val s = sample(hidden filter observed) {y => mln(y, weights)}
    for (p <- persons) {
      if (s.smokes(p)) smokeCounts(p) = smokeCounts.getOrElse(p, 0) + 1
      if (s.cancer(p)) cancerCounts(p) = cancerCounts.getOrElse(p, 0) + 1
    }
  }
  println("smokes: " + smokeCounts)
  println("cancer: " + cancerCounts)
}
