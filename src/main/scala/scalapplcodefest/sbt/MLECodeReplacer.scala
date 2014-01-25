package scalapplcodefest.sbt


/**
 * @author Sebastian Riedel
 */
class MLECodeReplacer(val env: GeneratorEnvironment) extends CodeStringReplacer with WolfePatterns {
  def replace(tree: env.global.Tree, modification: ModifiedSourceText): Boolean = {

    val replaced = env.replaceMethods(tree)
    val reduced = env.betaReduce(replaced)

    reduced match {
      case ApplyArgmin(_, _, vectors, _, LogLikelihood(data, domain, f, w), _)
        if f.symbol.hasAnnotation(MarkerOneHot) =>

        val featName = f.symbol.name.toString
        val sum = s"""sum2 ($data) (_ => true) { y_i => $featName(y_i) } mapValues(w => math.log(w / $data.size)) """
        modification.replace(tree.pos.start, tree.pos.end, sum)
        true
      case ApplyArgmin(_, _, _, _, _, _) =>
        println(replaced)
        println(reduced)
        false

      case _ =>
        false
    }
  }

}

object MLECodeReplacer {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/newExamples/CoinTossing.scala",
      replacers = List(new MLECodeReplacer(_)))
  }
}



