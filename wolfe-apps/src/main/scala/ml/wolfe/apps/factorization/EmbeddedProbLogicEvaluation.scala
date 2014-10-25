package ml.wolfe.apps.factorization

import java.io.File

import cc.factorie.la.DenseTensor1
import com.typesafe.config.ConfigFactory

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object EmbeddedProbLogicEvaluation {

  def main(args: Array[String]) {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl.conf"))

    def relationFilter(rel:String) = rel.startsWith("path") || (rel.startsWith("REL$") && rel != "REL$NA")

    //load raw data
    val trainRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.train")),relationFilter).take(10000)
    val train = FactorizationUtil.filterRows(trainRaw.toSeq, 10, 2)
    val trainRelations = train.flatMap(_.relations.map(_._1)).distinct.sorted
    val freebaseRelations = trainRelations.filter(_.startsWith("REL$"))

    val testRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.test")),relationFilter)
    val test = FactorizationUtil.filterRows(testRaw.toSeq, 0, 0,trainRelations.toSet -- freebaseRelations.toSet)

    println(trainRelations.size)
    println(train.size)
    println(test.size)

    println("Extracting rules")
    val trainRules = RuleLearner.pairwiseRules(train)

    println("Embedding rules")
    val ple = ProbLogicEmbedder.embed(trainRules)

    println("Prediction")
    val predictedRows = test map (row => ple.predictRow(row,freebaseRelations))
    val predictedFacts = FactorizationUtil.toRankedFacts(test zip predictedRows)

    println(predictedFacts.take(100).mkString("\n"))
    FactorizationUtil.saveForUSchemaEval(predictedFacts,new File("/tmp/ple.txt"))


    




  }

}

object EmbeddedProbLogicPlayground {

  def main(args: Array[String]) {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl-synth.conf")) withFallback ConfigFactory.parseFile(new File("conf/epl.conf"))
    implicit val random = new Random(0)

    val ruleProb = Map(
      (true,true) -> 0.2,
      (true,false) -> 0.0,
      (false,true) -> 0.4,
      (false,false) -> 0.4
    )

    val manualRules = Rules(Map(
      ("r1","r2")-> Rule("r1","r2",ruleProb),
      ("r2","r3")-> Rule("r2","r3",ruleProb),
      ("r3","r4")-> Rule("r3","r4",ruleProb)))

    val randomRows = FactorizationUtil.sampleRows(10,10,0.1)
    val randomRelations = randomRows.flatMap(_.relations.map(_._1)).distinct.sorted
    val randomRules = RuleLearner.pairwiseRules(randomRows)

    val pleManual = ProbLogicEmbedder.embed(manualRules)
    val pleRandom = ProbLogicEmbedder.embed(randomRules)

    val predictions = for (row <- randomRows) yield {
      row.copy(relations = randomRelations.map(r => r -> pleRandom.predict(row.observedTrue,r)))
    }

    println(FactorizationUtil.renderPredictions(predictions,randomRows))

  }
}




