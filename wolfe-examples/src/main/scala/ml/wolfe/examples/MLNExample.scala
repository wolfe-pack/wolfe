package ml.wolfe.examples

import ml.wolfe.Wolfe
import ml.wolfe.macros.OptimizedOperators

/**
 * @author Sebastian Riedel
 */
object MLNExample {

  import Wolfe._
  import OptimizedOperators._

  case class Person(name: Symbol)
  case class World(smokes: Pred[Person], cancer: Pred[Person], friends: Pred[(Person, Person)])

  val anna = Person('Anna)
  val bob  = Person('Bob)

  implicit def persons = List(anna, bob)
  def worlds = all(World)

  def mln(world: World) = {
    import world._
//    sum(persons) { p => 1.5 * I(smokes(p) --> cancer(p)) } +
    sum(persons) { p1 => sum(persons) { p2 => 1.1 * I(friends(p1, p2) --> (smokes(p1) == smokes(p2))) } }
  }

  def main(args: Array[String]) {
    def evidence(world: World) = true //  world.smokes(anna) && world.friends(anna, bob)
    def query(world: World) = oneHot(1) // oneHot(world.cancer(bob))
    val mu = expect(worlds where evidence) { mln } { query }

    println(mu)


  }

}