package ml.wolfe.examples

import ml.wolfe._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.util.Math._

/**
 * @author sameer
 * @since 5/23/15.
 */
object SmokesCancer {

  val p = Seq('Anna, 'Bob, 'Charlie)
  val friendsSet = Set(('Anna, 'Bob),('Anna, 'Charlie)).flatMap(x => Set(x,x.swap))

  implicit val Persons = p.toDom
  val persons = p.toConst
  @domain case class World(smokes: Pred[Symbol], cancer: Pred[Symbol])
  implicit val Worlds = World.Values(Preds(Persons), Preds(Persons))

  implicit val Friends = FullMaps(Persons, Persons, Bools)
  val friends = (for(p1 <- Persons.values; p2 <- Persons.values) yield (p1->p2)->friendsSet((p1,p2))).toMap.toConst

  /**
   * Questions:
   * - How can I constrain Maps not to have missing keys? (todo: sameer, add CompleteMaps)
   * - Get value => Domain.Const(value) to work implicitly? (todo: seb)
   * - Why doesn't argmax work for either res or res2? (lines 49 and 52) (todo: seb)
   */
  def score(w: Worlds.Term): DoubleTerm = {
    import w._
    sum(persons) {
      p => -2.0 * I(cancer(p))
    } + sum(persons) {
      p => 1.0 * I(smokes(p) --> cancer(p))
    } + sum(persons) { p1 => sum(persons) { p2 =>
      1.0 * I(friends(p1, p2) --> (smokes(p1) <-> smokes(p2)))
    }
    }
  }

  def main(args: Array[String]): Unit = {
    println(Worlds.size)
    for (w <- Worlds) {
      println(w + ": " + score(Worlds.Const(w)).eval())
    }
    val res = argmax(Worlds)(score).eval()
    println(res + ": " + score(Worlds.Const(res)).eval())
    val res2 = argmax(Worlds)(w => score(w) subjectTo(w.smokes(Persons.Const('Anna)) && w.smokes(Persons.Const('Bob))))
    println(res2.eval() + ": " + score(Worlds.Const(res2.eval())).eval())
  }

}
