package ml.wolfe.apps.factorization.util

import java.io.FileWriter

import ml.wolfe.apps.factorization.TensorDB
import ml.wolfe.apps.factorization.io.LoadNAACL
import ml.wolfe.util.Conf

import scala.util.Random

/**
 * @author rockt
 */
object DataInspector extends App {
  Conf.add(args.lift(0).getOrElse("./conf/mf.conf"))

  /*
  val db = LoadNAACL()
  println(db.toInfoString)
 
  println(db.trainCells.count(_.key1.toString.startsWith("REL$")))

  Mentions.load()
  val pathToMentionsMap = Mentions.pathToMentions

  val paths = Seq(
    "path#nn|<-nn<-unit->prep->of->pobj->|pobj:INV",
    "path#appos|->appos->producer->dep->|dep:INV",
    "path#nsubj|<-nsubj<-city->prep->in->pobj->|pobj",
    "path#pobj|<-pobj<-to<-prep<-move->prep->to->pobj->|pobj:INV"
  )

  paths.foreach(p => {
    println(p)
    println(pathToMentionsMap(p).mkString("\n"))
    println()
  })
  */

  val lengthsWriter = new FileWriter("./data/eval/lengths.txt")

  val formulaePredicates = {
    val db2 = LoadNAACL()
    db2.formulae.map(f => f.predicates(0) -> f.predicates(1))
  }
  
  def writeLenghts(pathToDB: String, sample: Boolean, label: String) {
    val db = new TensorDB(100)

    db.deserialize(pathToDB)

    val rand = new Random(0l)

    val numSamples = 1000

    val pairs = 
      if (sample) 
        for (i <- 0 until numSamples) yield {
          val premise = db.keys1(rand.nextInt(db.keys1.size))
          val consequent = db.keys1(rand.nextInt(db.keys1.size))
          (premise, consequent)
        }
      else 
        formulaePredicates

    //println(pairs.size)

    def key1ToLength(key1: Any): Double = db.node1(key1).get.variable.asVector.b.twoNorm


    pairs.foreach(p => {
      val (premise, consequent) = p
      lengthsWriter.write((key1ToLength(consequent) - key1ToLength(premise)).toString + "\t" + label + "\n")
    })
  }

  writeLenghts("data/out/F/serialized/", true, "mf-sample")
  writeLenghts("data/out/F/serialized/", false, "mf-formulae")
  writeLenghts("data/out/F-Pre/serialized/", false, "pre-formulae")
  writeLenghts("data/out/F-formulae-100/serialized/", false, "joint-formulae")

  lengthsWriter.close()
}
