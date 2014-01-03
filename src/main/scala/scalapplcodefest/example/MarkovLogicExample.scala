package scalapplcodefest.example
import scalapplcodefest._

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

    val smokes = data.smokes
    val cancer = data.cancer
    val friends = data.friends

    def f1 = vsum(persons) {p => ft('smokingIsBad, smokes(p) -> cancer(p))}

    def f2 = vsum(c(persons, persons)) {
      p =>
        ft('peerPressure, friends(p) -> (smokes(p._1) <-> smokes(p._2)))
    }

    (f1 + f2) dot weights
  }

  val weights = Vector('smokingIsBad -> 2.0, 'peerPressure -> 0.0)

  val hidden = for (smokes <- maps(persons, bools);
                    cancer <- maps(persons, bools);
                    friends <- maps(c(persons, persons), bools))
  yield Data(smokes, cancer, friends)

  //val prediction1 = argmax(hidden) {y => mln(y, weights)}

  val fr = Map(('Anna, 'Bob)-> true, ('Bob, 'Anna)-> false, ('Bob, 'Bob)-> false, ('Anna, 'Anna)-> false)
  println(fr == map(false, ('Anna, 'Bob) -> true))

  val prediction2 = argmax(hidden filter {h => h.smokes('Anna) && h.cancer('Anna) && h.friends == map(false, ('Anna, 'Bob) -> true)})(y => mln(y, weights))

  println(prediction2)

}
