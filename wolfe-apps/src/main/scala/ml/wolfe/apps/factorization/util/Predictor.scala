package ml.wolfe.apps.factorization.util

import ml.wolfe.apps.factorization.{DefaultIx, TensorKB}

/**
 * @author rockt
 */
object Predictor extends App {
  val pathToMatrix = args.lift(0).getOrElse("./data/out/bbc/serialized/")
  val relations = if (args.size > 1) args.tail else Array("path#|->nmod->per->pmod->involvement->nmod->about->sbj->|")

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
  predictions.foreach(t => t._2.take(100).foreach { case (score, es) =>
    val Array(e1,e2) = es.toString.tail.init.split(",")
    println(s"$score\t$e1\t$e2\t${t._1}")
  })
}
