package ml.wolfe.apps.factorization.io

import java.io.FileWriter


import ml.wolfe.nlp.util.ANSIFormatter._
import ml.wolfe.util.ProgressBar

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random


/**
 * @author rockt
 */

object Mentions {  
  val pathToMentions = new collection.mutable.HashMap[String, ArrayBuffer[String]]

  def load(filePath: String = "./wolfe-apps/data/naacl2013/nyt-freebase.test.mentions.txt"): Unit = {
    println("Loading sentences for dependency paths...")

    val progressBar = new ProgressBar(Source.fromFile(filePath, "iso-8859-1").getLines().size, 100000)
    progressBar.start()

    val lines = Source.fromFile(filePath, "iso-8859-1").getLines()
    lines.foreach(line => {
      if (!line.isEmpty && !line.startsWith("#Document")) {
        val splits = line.split("\t")
        val label = splits(0)
        val arg1 = splits(1)
        val arg2 = splits(2)
        val typ = splits(3)

        val path = splits.find(_.startsWith("path#")).get

        var sentence = splits.find(_.startsWith("sen#")).get.drop(4)
                       .replaceAllLiterally(arg1, arg1.onBlue())
                       .replaceAllLiterally(arg2, arg2.onRed())

        pathToMentions.getOrElseUpdate(path, new ArrayBuffer[String]()) += sentence
      }
      progressBar()
    })
  }
}

object Action extends Enumeration {
  type Answer = Value
  val No, Yes, Unsure, Opposite, More, Quit, Undefined = Value
}

object FormulaeAnnotator extends App {
  import Action._

  val filePath = args.lift(0).getOrElse("./wolfe-apps/data/formulae/1000.txt")
  val reannotate = args.lift(1).getOrElse("false").toBoolean
  val skipUntil = args.lift(2).getOrElse("0").toInt
  val rhsFilter: String => Boolean = if (args.size > 3) s => !s.endsWith(args(3)) else s => true

  println(s"Creating backup at $filePath.old...")
  val backup = new FileWriter(filePath + ".old")
  backup.write(Source.fromFile(filePath, "iso-8859-1").getLines().mkString("\n"))
  backup.close()
  val rand = new Random(0l)
  val fileWriter = new FileWriter(filePath + ".tmp")

  Mentions.load()
  val text = Source.fromFile(filePath, "iso-8859-1").getLines().mkString("\n")
  val formulae = text.split("\n\n")
  val newFile = new FileWriter(filePath)
  var quit = false

  formulae.foreach(formulaDatum => {
    val Array(statsTmp, formula) = formulaDatum.split("\n")

    val isCurated = statsTmp.endsWith("curated")
    val stats = if (isCurated) statsTmp.dropRight(8) else statsTmp
    val ix = stats.drop(2).split("\t")(0).toInt


    if (quit || (!reannotate && isCurated) || ix < skipUntil || !rhsFilter(formula)) {
      //ignore already curated formulae or the rest of the formulae in case we quit annotation
      fileWriter.write(formulaDatum + "\n\n")
    } else {
      //otherwise start annotation
      //currently only supports implications

      val Array(lhsTmp, arrow, rhsTmp) = formula.split(" ")

      val isCommentedOut = lhsTmp.startsWith("//")
      val isNegated = rhsTmp.startsWith("!")

      val lhs = if (isCommentedOut) lhsTmp.drop(2) else lhsTmp
      val rhs = if (isNegated) rhsTmp.drop(1) else rhsTmp

      val sentences = Mentions.pathToMentions.get(lhs)
                      .map(s => rand.shuffle(s)).getOrElse(List())

      println()
      println(stats)

      var path = lhs.drop(5)

      val pathWords = path
                      .split("\\|")(1)
                      .split("<-|->")
                      .zipWithIndex
                      .filter(_._2 % 2 == 0)
                      .map(_._1)
                      .toList
                      .filterNot(_.isEmpty)
                      .map(_.trim)

      for (w <- pathWords)
        path = path.replaceAllLiterally(w, w.onYellow())

      val rel = rhs.split("/").last

      println(path + " => " + rhs.replaceAllLiterally(rel, rel.onMagenta()))
      if (reannotate && isCurated)
        println("currently".onCyan() + ": " +
        (if (isCommentedOut) "//".onRed() else "") +
        "A" + " => " + (if (isNegated) "!".onRed() else "") + "B"
        )


      var ix = 0
      var answer: Answer = Undefined

      while (answer == Undefined) {
        var examples = sentences.slice(ix, ix+10).toList
        for (w <- pathWords.filter(_.size > 2)) {
          examples = examples.map(s => s.replaceAllLiterally(w, w.yellow()))
        }

        examples.foreach(s => println(s"\t$s"))
        ix += 10

        print("\nAdd this rule [y/n/u], the opposite [o] or show more mentions [m]: \r")

        answer = stringToAnswer(readLine())
        while (answer == Undefined) {
          print("Please answer with [y/n/o/u/m/quit]! \r")
          answer = stringToAnswer(readLine())
        }

        answer match {
          case Yes =>
            fileWriter.write(s"$stats\tcurated\n")
            fileWriter.write(s"$lhs => $rhs\n\n")
          case No =>
            fileWriter.write(s"$stats\tcurated\n")
            fileWriter.write(s"//$lhs => $rhs\n\n")
          case Opposite =>
            fileWriter.write(s"$stats\tcurated\n")
            fileWriter.write(s"$lhs => !$rhs\n\n")
          case Unsure =>
            fileWriter.write(s"$stats\n")
            fileWriter.write(s"//$lhs => $rhs\n\n")
          case More =>
            answer = Undefined
          case Quit =>
            fileWriter.write(formulaDatum + "\n\n")
            quit = true
        }
      }
    }
  })

  fileWriter.close()

  def stringToAnswer(string: String): Answer = string.toLowerCase.trim match {
    case "y" | "yes" => Yes
    case "n" | "no" => No
    case "o" | "opposite" => Opposite
    case "u" | "unsure" => Unsure
    case "m" | "more" => More
    case "quit" | "exit" => Quit
    case _ => Undefined
  }
  newFile.write(Source.fromFile(filePath + ".tmp", "iso-8859-1").getLines().mkString("\n"))
  newFile.close()
}

/*
object CompareRanks extends App {
  import RuleFinder.loadDB

  val db1Path = if (args.size > 0) args(0) else "./out/vectorland-F/serialized/"
  val db2Path = if (args.size > 1) args(1) else "./out/latest/serialized/" //"./out/vectorland-F-rules-100/serialized/"

  val rulesFile = if (args.size > 2) args(2) else db2Path+"rules.txt"

  val implRhsToLhsMap = Source.fromFile(rulesFile, "iso-8859-1").getLines().toList.map(_.split(" => "))
                        .collect { case Array(lhs,rhs) if !lhs.startsWith("//") && !rhs.startsWith("!") => (lhs, rhs) }
                        .groupBy(_._2).mapValues(l => l.map(_._1))

  val implNegRhsToLhsMap = Source.fromFile(rulesFile, "iso-8859-1").getLines().toList.map(_.split(" => "))
                           .collect { case Array(lhs,rhs) if !lhs.startsWith("//") && rhs.startsWith("!") => (lhs, rhs.drop(1)) }
                           .groupBy(_._2).mapValues(l => l.map(_._1))

  //println("A => B")
  //println(implRhsToLhsMap.mkString("\n"))

  //println("A => !B")
  //println(implNegRhsToLhsMap.mkString("\n"))

  val db1 = loadDB(db1Path)
  val db2 = loadDB(db2Path)

  def printChanges(formulaMap: Map[String, List[String]]) = {
    val changeInImplRanks = toChangeInRanksAndP(formulaMap)
    val changeInImplFormulae = toChangeInFormulaeSimAndScore(formulaMap)
    for {
      key <- changeInImplRanks.keys
      (rankChange, rankImproved, pChange) = changeInImplRanks(key)
      (simChange, scoreChange, scoreObsChange) = changeInImplFormulae(key)
    } println(key + "\n\trank: %8.2f\tups: %5.2f\tp: %5.2f\tscore: %5.2f\tsim: %5.2f"
                    .format(rankChange, rankImproved, pChange, scoreChange, simChange) + "\n")
  }

  def toChangeInRanksAndP(formulaMap: Map[String, List[String]]): Map[String, (Double, Double, Double)] = {
    formulaMap.toList.map(t => {
      val (key, values) = t

      val db1ImplFacts = sortByRHS(db1, key, formulaMap)
      val db2ImplFacts = sortByRHS(db2, key, formulaMap)

      val compared = compare(db1ImplFacts, db2ImplFacts).take(1000)

      //println(compared.mkString("\n"))

      val rankChange = compared.values.map(_._1).sum / compared.size.toDouble
      val pChange = compared.values.map(_._2).sum / compared.size
      val rankImproved = compared.values.map(_._1).count(_ > 0) / compared.size.toDouble

      //println(s"\tavg rank change: ${if (rankChange > 0) "+" + rankChange else rankChange}")
      //println(s"\tavg p change:    $pChange")
      key -> (rankChange, rankImproved, pChange)
    }).toMap
  }

  def sortByRHS(db: SPDB, relation: String, rhsToLhsMap: Map[String, List[String]]): Map[Fact, (Int, Double)] =
    getTopKFactsPerRel(db, relation).zipWithIndex
    .filter { case (f, ix) => existsPremise(db1, f, rhsToLhsMap) }
    .map { case (f, ix) => f -> (ix,  db.prob(f)) }
    .toMap

  def getTopKFactsPerRel(db: SPDB, relation: String, k: Int = Int.MaxValue): Seq[Fact] =
    db.facts(db.relation(relation).get).sortBy(f => -db.prob(f)).take(k)

  def existsPremise(db: SPDB, fact: Fact, rhsToLhsMap: Map[String, List[String]]): Boolean =
    rhsToLhsMap(fact.relation.name).exists(lhs => db.fact(fact.args, db.relation(lhs).get).isDefined)

  def compare(map1: Map[Fact, (Int, Double)], map2: Map[Fact, (Int, Double)]): Map[Fact, (Int, Double)] =
    (map1.keySet intersect map2.keySet).map(k => {
      val (ix1, p1) = map1(k)
      val (ix2, p2) = map2(k)
      k -> (ix1 - ix2, p2 - p1)
    }).toMap

  import RuleFinder.implScore
  import RuleFinder.implNegScore
  import RuleFinder.implScoreTruePremise
  import RuleFinder.implScoreTrain

  def toChangeInFormulaeSimAndScore(formulaMap: Map[String, List[String]]): Map[String, (Double, Double, Double)] = {
    formulaMap.toList.map(t => {
      val (key, values) = t
      var scoreChange = 0.0
      var scoreObsChange = 0.0
      var simChange = 0.0
      for {
        lhsName <- values
        rhsName = key
        rhsDb1 = db1.relation(rhsName).get
        lhsDb1 = db1.relation(lhsName).get
        rhsDb2 = db2.relation(rhsName).get
        lhsDb2 = db2.relation(lhsName).get
        lhsEmbeddingDb1 = lhsDb1.embedding(db1)
        rhsEmbeddingDb1 = rhsDb1.embedding(db1)
        lhsEmbeddingDb2 = lhsDb1.embedding(db2)
        rhsEmbeddingDb2 = rhsDb1.embedding(db2)
        db1Sim = lhsEmbeddingDb1 cosineSimilarity rhsEmbeddingDb1
        db2Sim = lhsEmbeddingDb2 cosineSimilarity rhsEmbeddingDb2
      } {
        val scoreDb1 = implScore(lhsDb1, rhsDb1, db1.trainFacts.map(_.args))(db1)._1
        val scoreDb2 = implScore(lhsDb2, rhsDb2, db2.trainFacts.map(_.args))(db2)._1

        val scoreObsDb1 = implScore(lhsDb1, rhsDb1, rowsWithTrueLhs(db1, lhsName))(db1)._1
        val scoreObsDb2 = implScore(lhsDb2, rhsDb2, rowsWithTrueLhs(db2, lhsName))(db2)._1

        simChange += (db2Sim - db1Sim)
        scoreChange += (scoreDb2 - scoreDb1)
        scoreObsChange += (scoreObsDb2 - scoreObsDb1)
      }
      key -> (simChange / values.size, scoreChange / values.size, scoreObsChange / values.size)
    }).toMap
  }

  def rowsWithTrueLhs(db: SPDB, lhs: String): Seq[List[Entity]] =
    db.facts(db.relation(lhs).get).filter(_.train).map(_.args)

  println("A => B")
  printChanges(implRhsToLhsMap)
  println("\nA => !B")
  printChanges(implNegRhsToLhsMap)
}
*/

object FormulaeFilter extends App {
  val filePath = args.lift(0).getOrElse("data/rules-curated-merged.txt")
  val outputPath = args.lift(1).getOrElse("data/rules-filtered.txt")
  val fileWriter = new FileWriter(outputPath)

  val rules = Source.fromFile(filePath, "iso-8859-1").getLines().mkString("\n").split("\n\n")
  for {
    rule <- rules
    Array(stats, formula) = rule.split("\n")
    tmp = stats.drop(2).split("\t")
    if tmp.size > 5
    Array(rank, dataProb, dataCount, mfProb, mfCount, "curated") = tmp
    if !formula.startsWith("//")
    if dataProb.toDouble < 0.9 && dataCount.toInt < 100 && mfProb.toDouble < 0.9
  } {
    fileWriter.write(rule + "\n\n")
  }

  fileWriter.close()
}











