package scalapplcodefest.sbt

import scala.tools.nsc.Global


/**
 * @author Sebastian Riedel
 */
class MLECodeReplacer(val global:Global) extends CodeStringReplacer with WolfePatterns {
  def replace(tree: global.Tree, modification: ModifiedSourceText): Boolean = {

    tree match {
      case ApplyArgmin(_, vectors, LogLikelihood(data, domain, f, w)) =>
        val featName = f.symbol.name.toString
        val sum = s"""sum ($data) { y_i => $featName(y_i) } mapValues(w => math.log(w / $data.size)) """
        modification.replace(tree.pos.start, tree.pos.end, sum)
        true
      case _ =>
        false
    }
  }

}

object MLECodeReplacer {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/sbt/CoinTossingToBeCompiled.scala",
      replacers = List(new MLECodeReplacer(_)))
  }
}

