package ml.wolfe.apps

import ml.wolfe.GibbsSampling
import ml.wolfe.Wolfe._
import ml.wolfe.macros.OptimizedOperators._
import ml.wolfe.D3Implicits._
import ml.wolfe.util.Timer

/**
 * Created by luke on 11/09/14.
 */
object MLN extends App {

  case class World(smokes: Pred[Symbol], cancer: Pred[Symbol],
                   friends: Pred[(Symbol, Symbol)])
  type PartiallyObservedWorld = World

  def testWorld(w:PartiallyObservedWorld) = {
    implicit val persons = (w.friends.keySet.flatMap(t => Seq(t._1, t._2)) ++ w.smokes.keySet ++ w.cancer.keySet).toSeq

    println(persons)
    println(w)

    //implicit def persons = List('Anna, 'Bob)
    def worlds = all(World)

    @LogZByInference(GibbsSampling(_))
    def mln(world: World) = {
      import world._
      sum(persons) { p =>
        1.5 * I(smokes(p) --> cancer(p)) } +
      sum(persons) { p1 => sum(persons) { p2 =>
        1.1 * I(friends(p1, p2) --> (smokes(p1) == smokes(p2))) } }
    }

    def query(world: World) = oneHot(world.cancer('Bob))


    Timer.time("foo") {

      val mu = expect(worlds where {world =>
        w.smokes.forall { entry => world.smokes(entry._1) == entry._2} &&
        w.cancer.forall { entry => world.cancer(entry._1) == entry._2 } &&
        w.friends.forall { entry => world.friends(entry._1) == entry._2 }
      }) { mln } { query }


      //val mu = expect(worlds) { mln } { query }


      println(mu.toString())

      //saveD3Graph(FactorGraphBuffer.get)
    }


    println(Timer.getTimeString(Timer.reported("foo")))
  }

  def worldSamples:Seq[PartiallyObservedWorld] = Seq(
    new PartiallyObservedWorld(Map('Anna -> true), Map(), Map('Anna -> 'Bob -> true))
  )

  worldSamples.foreach(testWorld)
}
