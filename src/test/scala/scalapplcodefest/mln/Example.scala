package scalapplcodefest.mln

import org.scalatest.{Matchers, FlatSpec}
import scalapplcodefest.legacy.term.State


/**
 * Created by larysa  04.12.13
 */
class MLNTest extends FlatSpec with Matchers {

  "MLN parsing" should "work" in {

    val mln_file = "scalapplcodefest/mln/social/network/smoking.mln"
    val db_file = "scalapplcodefest/mln/social/network/smoking-train.db"
    val result_file = "scalapplcodefest/mln/social/network/smoking.result"

    val mln = new MLNTranslator
    mln.mln(mln_file).db(db_file) /*.query("Cancer", "Friends", "Smokes")*/ .getModel
    val state: State = mln.getState
    println("state = " + state)

    // the code below is just an example
    //    val feats: Seq[(Seq[Symbol], Double)] = folf map (f => (Seq(Symbol(f._1)), f._2))
    //    val concreteWeights = index.createDenseVector(feats.toList: _*)()
    //
    //    val condition = getState
    //
    //    val conditioned = mln | condition | weights -> concreteWeights
    //
    //    val queryPredicates: Set[Predicate[_, Boolean]] = queryPredicate map (q => predicates.get(Symbol(q)).get)
    //    val signature: Sig[_] = queryPredicates.toList match {
    //      case List(x) => throw new scala.Error("error: we do not provide sig for a one argument " + x)
    //      case List(x, y) => sig(x.asInstanceOf[Predicate[Any, Boolean]], y.asInstanceOf[Predicate[Any, Boolean]])
    //      case List(x, y, z) => sig(x.asInstanceOf[Predicate[Any, Boolean]], y.asInstanceOf[Predicate[Any, Boolean]], z.asInstanceOf[Predicate[Any, Boolean]])
    //    }
    //

    //    val lambda: LambdaAbstraction[_, Double] = lam(signature, conditioned)
    //    val maxi: RichMax[_] = max(lambda)
    //    val argmax = argState(maxi.byMessagePassing()).value()
    //

    //    println(argmax)


  }


  "MLN structure learning" should "work" in {

    val mln_file = "scalapplcodefest/mln/social/network/learning/smoking-str-learn.mln"
    val db_file = "scalapplcodefest/mln/social/network/learning/smoking-train-str-learn.db"

    val mln = new MLNTranslator
    mln.mln(mln_file).db(db_file).justTransform
    val rawState: List[GroundAtom[_, Boolean]] = mln.getRawState

    // todo: to many args
    StructLearner(mln.getDomains,
    /*do we need mln.getPredicates here? consider to remove*/
      mln.getPredicates,
      mln.getRawState,
      mln.getPredicatesDictionary).getGraph
  }

}


