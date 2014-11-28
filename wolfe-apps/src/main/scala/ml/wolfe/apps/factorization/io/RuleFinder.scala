package ml.wolfe.apps.factorization.io

import java.io.FileWriter

import ml.wolfe.apps.factorization.{Formula, Impl, ImplNeg, TensorKB}
import ml.wolfe.util.{ProgressBar, Conf}

import scala.util.Random


/**
 * @author rockt
 */
object RuleFinder extends App {
  type Rule = Formula
  type Entity = Any
  type Relation = Any
  type SPDB = TensorKB

  def formulaScore(rule: Rule, pairs: Seq[List[Entity]], argFilter: List[Entity] => Boolean = e => true)(implicit db: SPDB): (Double, Int) = {
    val rows = pairs.filter(argFilter)
    (rows.map(e => rule(e)).sum / rows.size, rows.size)
  }

  def formulaScoreObserved(rule: Rule, pairs: Seq[List[Entity]], argFilter: List[Entity] => Boolean = e => true)(implicit db: SPDB): (Double, Int) = {
    val p1 = rule.predicates(0)
    val rows = db.getBy1(p1).map { case (ei, ej) => List(ei, ej) }
    val filteredRows = rows.filter(argFilter)
    (filteredRows.map(e => rule(e)).sum / filteredRows.size, filteredRows.size)
  }

  def implScore(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScore(Impl(r1,r2), pairs)

  def implNegScore(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScore(ImplNeg(r1,r2), pairs)

  def implScoreObserved(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScoreObserved(Impl(r1,r2), pairs)

  def implNegScoreObserved(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScoreObserved(ImplNeg(r1,r2), pairs)
  
  lazy val testRelations = Seq(
    "person/company",
    "location/containedby",
    "person/nationality",
    "author/works_written",
    "parent/child",
    "person/place_of_birth",
    "person/place_of_death",
    "neighborhood/neighborhood_of",
    "person/parents",
    "company/founders",
    "sports_team/league",
    "team_owner/teams_owned",
    "team/arena_stadium",
    "film/directed_by",
    "roadcast/area_served",
    "structure/architect",
    "composer/compositions",
    "person/religion",
    "film/produced_by"
  ).toSet

  def consequentFilter(r: Relation) = testRelations.exists(s => r.asInstanceOf[String].contains(s))


  implicit val db = new SPDB

  db.deserialize(args.lift(0).getOrElse("./data/out/latest/serialized/"))

  //println(db.toVerboseString())

  val premises = db.relations
  val consequents = db.relations.filter(consequentFilter)

  val rows = db.trainCells.map(_.key2).distinct.map { case (ei, ej) => List(ei, ej) }

  println("Generating formulae...")
  val progressBar = new ProgressBar(consequents.size * premises.size)
  progressBar.start()
  
  val potentialRules = for {
    consequent <- consequents
    premise <- premises
    if premise != consequent
  } yield {
    val (score, numPremises) = implScoreObserved(premise, consequent, rows)
    progressBar.apply(consequent.toString)
    (score, numPremises, premise, consequent)
  }

  println("Writing formulae...")
  val ruleWriter = new FileWriter("data/formulae/rules-new.txt")
  potentialRules
  .filter(_._2 >= 5)
  .sortBy(-_._1)
  //.take(1000)
  .zipWithIndex
  .foreach(z => {
    val (t, ix) = z
    ruleWriter.write("//%d\t%.2f\t%d\n".format(ix, t._1, t._2))
    ruleWriter.write(s"${t._3} => ${t._4}\n\n")
  })
  ruleWriter.close()
}