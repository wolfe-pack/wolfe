package ml.wolfe.macros

import OptimizedWolfe._
import ml.wolfe.Wolfe
import Wolfe._

/**
 * @author Sebastian Riedel
 */
object TestSmokes {

  def main(args: Array[String]) {
    case class Person(first: String, last: String)
    case class Data(smokes: Pred[Person], cancer: Pred[Person], friends: Pred[(Person,Person)])

    val anna = Person("Anna", "F.")
    val bob = Person("Bob", "B.")

    implicit val allPersons = Seq(anna, bob)
    implicit val allData = all(Data)

    println(allData.size)

    def model(x: Data) =
      sumOld(allPersons)(_ => true) {p => I(x.smokes(p) --> x.cancer(p))}

    //todo: not sure why this doesn't work
    //val actual = OptimizedWolfe.argmax(allData)(x => x.smokes(anna))(model)
    val expected = BruteForceWolfe.argmax(allData)(x => x.smokes(anna))(model)

    println(expected)
    //println(actual)

    println(model(expected))
    //println(model(actual))



  }
}
