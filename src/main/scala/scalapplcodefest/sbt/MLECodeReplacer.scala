package scalapplcodefest.sbt


/**
 * @author Sebastian Riedel
 */
class MLECodeReplacer(val env:GeneratorEnvironment) extends CodeStringReplacer with WolfePatterns {
  def replace(tree: env.global.Tree, modification: ModifiedSourceText): Boolean = {

    tree match {
      case ApplyArgmin(_, vectors, LogLikelihood(data, domain, f, w)) =>
        val featName = f.symbol.name.toString
        val sum = s"""sum ($data) { y_i => $featName(y_i) } mapValues(w => math.log(w / $data.size)) """
        modification.replace(tree.pos.start, tree.pos.end, sum)
        true
      case ApplyArgmin2(_, vectors, _, LogLikelihood2(data, domain, f, w),_) =>
        val featName = f.symbol.name.toString
        val sum = s"""sum2 ($data) (_ => true) { y_i => $featName(y_i) } mapValues(w => math.log(w / $data.size)) """
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
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/CoinTossing2.scala",
      replacers = List(new MLECodeReplacer(_)))
  }
}

