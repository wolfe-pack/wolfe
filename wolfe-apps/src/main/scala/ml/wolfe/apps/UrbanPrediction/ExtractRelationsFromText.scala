package ml.wolfe.apps.UrbanPrediction

/**
 * Created by marziehsaeidi on 27/11/2014.
 */
object ExtractRelationsFromText {
  // Part1 : Extract POS patterns

  def main(args: Array[String]) = extractToBeAndAdjective(null)
  /**
   * Pattern: Location + VBZ(to be) + [Adv] + Adj
   * Example: Brixton is nice
   */
  def extractToBeAndAdjective(sentences:Array[String]){
    val text = "I<PRP> think<VBP> Brixton<LOCATION-NNP> is<VBZ> a<DT> very<RB> nice<JJ> area<N>"
    val regex = """.* ([^<]+)<LOCATION-NNP> ([^<]+)<VBZ>.* ([^< ]+)<JJ>.*""".r
    text match {
      case regex(area, verb, adj) => {
        println("Found %s described as %s".format(area, adj))
      }
      case _=> println("Not Found")
    }
  }
}
