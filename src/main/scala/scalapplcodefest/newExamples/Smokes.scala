package scalapplcodefest.newExamples

import scalapplcodefest.sbt.{MPGraphReplacerNew, GenerateSources, Compile}
import scalapplcodefest.Wolfe

/**
 * @author Sebastian Riedel
 */
@Compile
class Smokes extends (() => Unit) {

  import Wolfe._

  def apply() = {
    case class Person(first: String, last: String)
    case class Data(smokes: Pred[Person], cancer: Pred[Person], outbreak: Boolean)

    val anna = Person("Anna", "F.")
    val bob = Person("Bob", "B.")

    implicit val allPersons = Set(anna, bob)
    implicit val allData = all(Data)

    def model(data: Data) = {
      import data._
      -1.3 * I(outbreak) + sum(allPersons)(_ => true) {p => I(smokes(p) --> cancer(p))}
    }

    val mpe = argmax(allData)(d => d.smokes(anna))(model)

    println(mpe)

  }
}

object Smokes {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/Smokes.scala",
      replacers = List(env => new MPGraphReplacerNew(env)))
  }
}
