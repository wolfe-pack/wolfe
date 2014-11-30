package ml.wolfe.apps.factorization.util

import java.io.FileWriter

import ml.wolfe.apps.factorization.{Formula, Impl, ImplNeg, TensorKB}
import ml.wolfe.util.ProgressBar


/**
 * @author rockt
 */
object FormulaeExtractor extends App {
  type Rule = Formula
  type Entity = Any
  type Relation = Any
  type SPDB = TensorKB

  def formulaScore(rule: Rule, pairs: Seq[List[Entity]], argFilter: List[Entity] => Boolean = e => true)(implicit db: SPDB): (Double, Int) = {
    val rows = pairs.filter(argFilter).map(_.head)
    (rows.map(e => rule(e)).sum / rows.size, rows.size)
  }

  /**
   * Calculates the weight of the formula based on matrix factorization predictions.
   */
  def formulaScoreMF(rule: Rule, pairs: Seq[List[Entity]], argFilter: List[Entity] => Boolean = e => true)(implicit db: SPDB): (Double, Int) = {
    val p1 = rule.predicates(0)
    val rows = db.getBy1(p1).map { case (ei, ej) => List(ei) }

    //we only care about the score over true observed premises
    val filteredRows = rows.filter(argFilter).map(_.head).filter(e => {
      val cell = db.get(p1,e).get
      cell.train && cell.target == 1.0
    })

    (filteredRows.map(e => rule(e)).sum / filteredRows.size, filteredRows.size)
  }

  /**
   * Calculates the weight of the formula based on the training data.
   */
  def formulaScoreData(rule: Rule, pairs: Seq[List[Entity]], argFilter: List[Entity] => Boolean = e => true)(implicit db: SPDB): (Double, Int) = {
    val p1 = rule.predicates(0)
    val rows = db.getBy1(p1).map { case (ei, ej) => List(ei) }

    //we only care about the score over true observed premises
    val filteredRows = rows.filter(argFilter).map(_.head).filter(e => {
      val cell = db.get(p1,e).get
      cell.train && cell.target == 1.0
    })

    (filteredRows.map(e => rule match {
      case Impl(_, p2, _) => if (db.get(p1, e).get.target == 1.0 && db.get(p2, e).map(_.target).getOrElse(0.0) == 0.0) 0.0 else 1.0
      case ImplNeg(_, p2, _) => if (db.get(p1, e).get.target == 1.0 && db.get(p2, e).map(_.target).getOrElse(0.0) == 1.0) 0.0 else 1.0
    }).sum / filteredRows.size, filteredRows.size)
  }

  def implScore(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScore(Impl(r1,r2), pairs)

  def implNegScore(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScore(ImplNeg(r1,r2), pairs)

  def implScoreMF(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScoreMF(Impl(r1,r2), pairs)

  def implNegScoreMF(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScoreMF(ImplNeg(r1,r2), pairs)

  def implScoreData(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScoreData(Impl(r1,r2), pairs)

  def implNegScoreData(r1: Relation, r2: Relation, pairs: Seq[List[Entity]])(implicit db: SPDB): (Double, Int) =
    formulaScoreData(ImplNeg(r1,r2), pairs)



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

  println("Loading db...")
  db.deserialize(args.lift(0).getOrElse("wolfe-apps/data/out/F/serialized/"))
  println(db.toInfoString)

  val premises = db.relations
  //val consequents = db.relations.filter(consequentFilter)
  val consequents = db.relations

  val rows = db.trainCells.map(_.key2).distinct.map(List(_))
    //.map { case (ei, ej) => List(ei, ej) }

  println("Generating formulae...")
  val progressBar = new ProgressBar(consequents.size * premises.size, 1000)
  progressBar.start()
  
  val potentialRules = for {
    consequent <- consequents
    premise <- premises
    if premise != consequent
  } yield {
    val (scoreMF, numPremisesMF) = implScoreMF(premise, consequent, rows)
    val (scoreData, _) = implScoreData(premise, consequent, rows)
    progressBar.apply(consequent.toString)
    (scoreMF, scoreData, numPremisesMF, premise, consequent)
  }

  println()
  println("Writing formulae...")
  val ruleWriter = new FileWriter("wolfe-apps/data/formulae/latest.txt")
  potentialRules
  //.filter(_._2 >= 0.9)
  .filter(_._3 >= 10)
  .sortBy(-_._1)
  //.sortBy(-_._2)
  .take(100000)
  .zipWithIndex
  .foreach(z => {
    val (t, ix) = z
    ruleWriter.write("//%d\t%.2f\t%.2f\t%d\n".format(ix + 1, t._1, t._2, t._3))
    ruleWriter.write(s"${t._4} => ${t._5}\n\n")
  })
  ruleWriter.close()
}