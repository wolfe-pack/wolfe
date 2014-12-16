package ml.wolfe.apps.factorization.util

import java.io.FileWriter

import ml.wolfe.apps.factorization.hack.EntityHackNormalization
import ml.wolfe.apps.factorization.{DefaultIx, TensorKB}

/**
 * @author rockt
 */
object Predictor extends App {
  val pathToMatrix = args.lift(0).getOrElse("./data/out/bbc/serialized/")
  val outFile = args.lift(1).getOrElse("./data/out/bbc/predictions.txt")
  val relations = if (args.size > 2) args.tail else Array(
    "REL$/location/administrative_division/country",
    "REL$/base/biblioness/bibs_location/country",
    "REL$/location/location/contains",
    "REL$/people/person/nationality",
    "REL$/base/aareas/schema/administrative_area/administrative_parent",
    "REL$/location/country/first_level_divisions",
    "REL$/location/country/capital"
  )

  println("Loading db...")
  val kb = new TensorKB(100)
  kb.deserialize(pathToMatrix)

  println(kb.toInfoString)

  println("Predicting facts...")
  val predictions = relations.map(rel => rel -> kb.keys2
    .filterNot(t => kb.getFact(rel, t, DefaultIx).exists(_.train))
    .map(t => {
      (kb.prob(rel, t), t)
    }).sortBy(-_._1)
  ).toMap

  println("Reporting predictions...")

  if (true || args.size > 1) {

    val writer = new FileWriter(outFile)

    EntityHackNormalization.init()

    predictions.foreach(t => t._2.take(100).foreach { case (score, es) =>
      val Array(e1, e2) = es.toString.tail.init.split(",")
      val can1 = if (e1.startsWith("/m/")) EntityHackNormalization.getCanonical(e1) else e1
      val can2 = if (e2.startsWith("/m/")) EntityHackNormalization.getCanonical(e2) else e2

      writer.write(s"$score\t$e1\t$can1\t$e2\t$can2\t${ t._1 }\n")
    })
    writer.close()
  } else {
    predictions.foreach(t => t._2.take(100).foreach { case (score, es) =>
      val Array(e1, e2) = es.toString.tail.init.split(",")
      println(s"$score\t$e1\t$e2\t${ t._1 }")
    })
  }
}
