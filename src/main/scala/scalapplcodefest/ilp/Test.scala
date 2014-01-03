package scalapplcodefest.ILP

/**
 * Created by Jan on 02.01.14.
 */
object Test {
  type Person = String

  case class Observed(friends: (Person, Person) => Boolean)
  case class Hidden(smokes: Person => Boolean, cancer: Person => Boolean)

  val persons = Set('Anna, 'Bob)

  def mln(friends: Observed, hidden: Hidden, weights: Vector) = {

    val smokes = hidden.smokes
    val cancer = hidden.cancer

    def f1 = sum (persons) { p => feat(1, indicator(smokes(p) -> cancer(p))) }

    def f2 = sum (cartesian(persons, persons)) { p =>
      friends(p._1, p._2) -> (smokes(p._1) <-> smokes(p._2)) }

    (f1 + f2) dot weights
  }


  def main(args: Array[String]) {


    // observation
    val f  = p => p match {
      case ('Anna, 'Bob) => true
      case _ => false
    }

    val weights = Vector(2.0, 0)

    val hidden = for(smokes <- funs(persons, bools);
                     cancer <- funs(persons, bools))
    yield Hidden(smokes, cancer)

    val prediction1 = argmax hidden { y => mln(fr, y, weights)}

    val prediction2 = argmax (hidden filter {h => h.smokes('Anna) && h.cancer('Anna)}) (y => mln(fr, y, weights))

  }
}
