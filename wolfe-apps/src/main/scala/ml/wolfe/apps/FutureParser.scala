package ml.wolfe.apps
import ml.wolfe.nlp.io.CoNLLReader

/**
 * Created by narad on 11/26/14.
 */
object FutureParser extends App {

  val train = new CoNLLReader(args(0))

  for (t <- train) println(t.toCoNLLString + "\n")

}
