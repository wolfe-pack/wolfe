package ml.wolfe.examples

import ml.wolfe.macros.OptimizedOperators
import ml.wolfe.{D3Implicits, BeliefPropagation, BruteForceOperators, BruteForce}
import ml.wolfe.Wolfe._
import ml.wolfe.macros.OptimizedOperators._
import ml.wolfe.util.Timer
import org.scalameter.{Gen, PerformanceTest}
import D3Implicits._

object Scratch extends App {


  val ranges = (1 +: (1 to 100)).map(0 to _)
  /*
  val sizes = Gen.range("size")(1, 10, 1)

  val ranges = for {
    size <- sizes
  } yield 1 to size



  measure method "runExperiment" in {
    using(ranges) in */

  ranges.foreach {
    r =>

      def runExperiment = {
        case class World(smokes: Pred[Symbol], cancer: Pred[Symbol],
                         friends: Pred[(Symbol, Symbol)])

        implicit def persons = r.map(i => Symbol(i.toString)).toList
        def worlds = all(World)

        //@Atomic
        def smokingCausesCancer(w: World): Double = {
          sum(persons) { p =>
            1.5 * I(w.smokes(p) --> w.cancer(p))
          }
        }

        //@Atomic
        def smokingIsInfectious(w: World): Double = {
          sum(persons) { p1 => sum(persons) { p2 =>
            1.1 * I(w.friends(p1, p2) --> (w.smokes(p1) == w.smokes(p2)))
          }}
        }

        //@Atomic

        @LogZByInference(BeliefPropagation.sumProduct(-1))
        def mln(world: World) = {
          smokingCausesCancer(world) + smokingIsInfectious(world)
        }


        def query(world: World) = oneHot(world.cancer(persons.head))
        print(r.last + "\t")
        Timer.time("foo") {
          //BruteForceOperators.
          OptimizedOperators.expect(worlds) { mln } { w => oneHot('smokes, I(w.smokes(Symbol("0")))) }
        }
        println(Timer.reported("foo"))
        //print(r.last + "\t")
      }
      runExperiment
  }
  /*}*/
}
