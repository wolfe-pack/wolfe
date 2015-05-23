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

  @domain case class World(smokes: Pred[Symbol], cancer: Pred[Symbol])

  val persons = Seq('Anna, 'Bob, 'Charlie).toDom

  val friends = Set(('Anna, 'Bob),('Anna, 'Charlie)).flatMap(x => Set(x,x.swap))

  implicit val Worlds = World.Values(Preds(persons), Preds(persons))

  /**
   * Questions:
   * - Do we need persons.Const() everywhere? (lines 35-39)
   * - Do we need Bools.Const() (line 39)
   * - Why do Maps have missing keys? Can we create a separate function for all possible predicates?
   * - How can I constrain Maps not to have missing keys?
   * - How do I score the argmax state? see line 50, why doesn't score(res) work?
   * - How do I constrain the domain, instead of constraining the objective (using subjectTo)?
   * - Why doesn't argmax work for either res or res2? (lines 49 and 52)
   */
  def score(w: Worlds.Term): DoubleTerm = {
    import w._
    sum(persons.values) {
      p => 12.0 * I(cancer(persons.Const(p)))
    } + sum(persons.values) {
      p => 1.0 * I(smokes(persons.Const(p)) --> cancer(persons.Const(p)))
    } + sum(persons.values) { p1 => sum(persons.values) { p2 =>
      1.0 * I(Bools.Const(friends(p1, p2)) --> (smokes(persons.Const(p1)) <-> smokes(persons.Const(p2))))
    }
    }
  }

  def main(args: Array[String]): Unit = {
    println(Worlds.size)
    for (w <- Worlds) {
      println(w + ": " + score(Worlds.Const(w)).eval())
    }
    val res = argmax(Worlds)(score)
    println(res.eval() + ": " + score(Worlds.Const(res.eval())).eval())
    val res2 = argmax(Worlds)(w => score(w) subjectTo(w.smokes(persons.Const('Anna)) && w.smokes(persons.Const('Bob))))
    println(res2.eval() + ": " + score(Worlds.Const(res2.eval())).eval())
  }

}
