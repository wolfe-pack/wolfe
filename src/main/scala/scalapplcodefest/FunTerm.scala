package scalapplcodefest

/**
 * FunTerms evaluate to partial functions. The domain where the function is defined for
 * may depend on the state, and is hence a term itself.
 * The general contract for partial functions with the given function domain and target set are as follows:
 * the function is undefined for all values outside the domain, and every result is part of the target set. Note that
 * the function may also be undefined for some values inside the (super) domain.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
trait FunTerm[A, B] extends Term[Fun[A, B]] {
  def superDomain: Term[Set[A]]
  def targetSet: Term[Set[B]]
}

/**
 * Helper object to build FunTerms
 */
object FunTerm {
  /**
   * Returns term that corresponds to creating the set of all possible functions
   * given a domain and range.
   */
  def allFunctions[A, B] = ConstantFun(new AllFunctionsOp[A, B])
}

/**
 * Partial functions with invariant argument and result types. This is important for pattern matching.
 * @tparam A argument type.
 * @tparam B return type.
 */
trait Fun[A, B] extends PartialFunction[A, B] {
  def superDomain: Set[A]
  def domain: Set[A] = superDomain.filter(isDefinedAt)
  //TODO: would like to make this lazy
  def targetSet: Set[B]

}

/**
 * Helper object to build Fun objects.
 */
object Fun {
  def apply[A, B](f: PartialFunction[A, B], dom: Set[A] = new AllOfType[A], ran: Set[B] = new AllOfType[B]) = new Fun[A, B] {
    def apply(v1: A) = f.apply(v1)
    def isDefinedAt(x: A) = dom(x) && f.lift(x).exists(ran(_))
    def superDomain = dom
    def targetSet = ran
  }

  def empty[A, B] = new Fun[A, B] {
    def superDomain = Set.empty
    def targetSet = Set.empty
    def apply(v1: A) = sys.error(s"Empty function not defined at $v1")
    def isDefinedAt(x: A) = false
  }

}

/**
 * Helper to create constant function terms typed as FunTerm.
 */
object ConstantFun {
  def apply[A, B](fun: Fun[A, B]) = new Constant(fun) with FunTerm[A, B] {
    def superDomain = Constant(fun.superDomain)
    def targetSet = Constant(fun.targetSet)
  }
  def unapply[A, B](term: FunTerm[A, B]) = term match {
    //case Constant(fun) => Option(fun)
    case _ => None
  }
}


/**
 * Application of a function to an argument
 * @param function the function to apply
 * @param arg the argument to apply the function to
 * @tparam A argument type of function
 * @tparam B return type of function
 */
case class FunApp[A, B](function: FunTerm[A, B], arg: Term[A]) extends Term[B] {
  def eval(state: State) =
    for (f <- function.eval(state);
         a <- arg.eval(state);
         v <- f.lift(a)) yield v
  def variables = function match {
    //case Predicate(_, dom, ran) => ???
    case _ => SetUtil.SetUnion(List(function.variables, arg.variables))
  }
  def default = function.default(function.superDomain.default.head)
  def domain[C >: B] = Image(function, arg.domain).asInstanceOf[Term[Set[C]]]

}

/**
 * A Lambda Abstraction corresponds to a function and consists of a variable and a term.
 * The functions value at a particular argument can be determined by evaluating the inner
 * term with the variable bound to the given argument.
 *
 * @param variable the variable that corresponds to the argument of the function.
 * @param term the term that when evaluated becomes the return value of the function.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
case class LambdaAbstraction[A, B](variable: Variable[A], term: Term[B]) extends FunTerm[A, B] {

  lambda =>

  def superDomain = variable.domain
  def targetSet = term.domain[B]
  def eval(state: State) = {
    for (r <- targetSet.eval(state); d <- superDomain.eval(state)) yield new Fun[A, B] {
      def superDomain = d
      def targetSet = r
      def get(a: A) = term.eval(state + SingletonState(variable, a))
      def apply(a: A) = get(a).get
      override def isDefinedAt(a: A) = d(a) && get(a).exists(r(_))
    }
  }
  def variables = SetUtil.SetMinus(term.variables, Set(variable))
  def default = new Fun[A, B] {
    def superDomain = lambda.superDomain.default
    def targetSet = lambda.targetSet.default
    def apply(v1: A) = term.default
    def isDefinedAt(x: A) = variable.domain.default(x)
  }
  def domain[C >: Fun[A, B]] =
    FunApp(FunTerm.allFunctions[A, B], TupleTerm2(superDomain, targetSet)).asInstanceOf[Term[Set[C]]]
}

/**
 * Set of a functions from domain to range
 * @param domain the domain of the functions
 * @param range the range of the functions
 * @tparam A argument type.
 * @tparam B return type.
 */
case class AllFunctions[A, B](domain: Set[A], range: Set[B]) extends SetValue[Fun[A, B]] {

  self =>

  def contains(elem: Fun[A, B]) = elem.domain == domain && elem.targetSet == range
  def iterator = {
    def allFunctions(d: List[A], r: List[B], funs: List[Fun[A, B]] = List(Fun.empty)): List[Fun[A, B]] = {
      d match {
        case Nil => funs
        case newArg :: tail =>
          val newFunctions = for (v <- r; f <- funs) yield new Fun[A, B] {
            def superDomain = self.domain
            def targetSet = range
            override def domain = self.domain
            def apply(v1: A) = if (v1 == newArg) v else f(v1)
            def isDefinedAt(x: A) = x == newArg || f.isDefinedAt(x)
          }
          allFunctions(tail, r, newFunctions)
      }
    }
    allFunctions(domain.toList, range.toList).iterator
  }
}

class AllFunctionsOp[A, B] extends Fun[(Set[A], Set[B]), Set[Fun[A, B]]] {
  def superDomain = new AllOfType[(Set[A], Set[B])]
  override def domain = superDomain
  def targetSet = new AllOfType[Set[Fun[A, B]]]
  def isDefinedAt(x: (Set[A], Set[B])) = true
  def apply(v1: (Set[A], Set[B])) = AllFunctions(v1._1, v1._2)
}


/**
 * The Image of a function term is the set of return values we get by applying
 * the function to all elements of its domain.
 * @param fun the function to get the image for
 * @tparam A argument type of function.
 * @tparam B return type of function.
 */
case class Image[A, B](fun: FunTerm[A, B], dom: Term[Set[A]]) extends Term[Set[B]] {
  def eval(state: State) = for (f <- fun.eval(state); d <- dom.eval(state)) yield SetUtil.SetMap(d, f)
  def variables = SetUtil.SetUnion(List(fun.variables, dom.variables))
  def domain[C >: Set[B]] = Constant(Util.setToBeImplementedLater[C])
  def default = fun.default.targetSet
}

/**
 * The ImageSeq of a function term is a sequence of return values we get by applying
 * the function to all elements of its domain (in some undefined order). Compared to the image
 * of a function, here return values can be repeated.
 * @param fun the function to get the image for
 * @tparam A argument type of function.
 * @tparam B return type of function.
 */
case class ImageSeq[A, B](fun: FunTerm[A, B], dom: Term[Set[A]]) extends Term[Seq[B]] {
  def eval(state: State) = for (f <- fun.eval(state); d <- dom.eval(state)) yield d.view.toSeq.map(f)
  def variables = fun.variables
  def domain[C >: Seq[B]] = Constant(new AllOfType[C])
  def default = fun.default.targetSet.toSeq
}

case class ImageSeqCurried2[A1, A2, B](fun: FunTerm[A1, Fun[A2, B]]) extends Term[Seq[B]] {
  def eval(state: State) = for (f <- fun.eval(state); d <- fun.superDomain.eval(state)) yield {
    for (a1 <- d.view.toSeq; f1 = f(a1); a1 <- f1.domain.view.toSeq) yield f1(a1)
  }
  def variables = fun.variables
  def domain[C >: Seq[B]] = Constant(new AllOfType[C])
  def default = fun.default.targetSet.head.targetSet.view.toSeq
}


trait UncurriedLambdaAbstraction[A1, R] {
  def lambda1: LambdaAbstraction[A1, _]
  def lambdaLast: LambdaAbstraction[_, R]
  def variables = lambda1.variables
  def targetSet = lambdaLast.targetSet
}

object Curried2 {
  def unapply[A1, A2, R](f: FunTerm[A1, Fun[A2, R]]) = {
    f match {
      case l1@LambdaAbstraction(v1, l2@LambdaAbstraction(v2, term)) => Some(UncurriedLambdaAbstraction2[A1, A2, R](l1, l2))
      case _ => None
    }
  }


}

case class UncurriedLambdaAbstraction2[A1, A2, R](lambda1: LambdaAbstraction[A1, Fun[A2, R]],
                                                  lambdaLast: LambdaAbstraction[A2, R])
  extends FunTerm[(A1, A2), R] with UncurriedLambdaAbstraction[A1, R] {

  def eval(state: State) = for (f <- lambda1.eval(state)) yield Fun({
    case (a1, a2) => f(a1)(a2)
  })
  def domain[C >: Fun[(A1, A2), R]] = Constant(Util.setToBeImplementedLater)
  def default = Fun({
    case (a1, a2) => lambda1.default(a1)(a2)
  })
  def superDomain = CartesianProductTerm2(lambda1.variable.domain, lambdaLast.variable.domain)
}
