package ml.wolfe.apps.factorization.util

import ml.wolfe.apps.factorization.io.LoadNAACL
import ml.wolfe.util.Conf

/**
 * @author rockt
 */
object DataInspector extends App {
  Conf.add(args.lift(0).getOrElse("./conf/mf.conf"))
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
}
