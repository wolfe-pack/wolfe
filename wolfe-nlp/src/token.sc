//User
case class Sentence(words:Seq[String],tags:Seq[String])
trait MyList[T]
case class Cons[+T](h:T, tail:MyList[T]) extends MyList
case object MyNil extends MyList[Nothing]


def space = (for (w <- List("A","B"); t <- List("A","B")) yield Sentence(w,Nil))
SentenceTerm(MyNil,MyNil)


def get[T](index:Int,list:MyList[T]):T = list match {
  case MyNil => sys.error("")
  case Cons(h,t) => index match {
    case 0 => h
    case n => get(n-1,t)
  }
}

trait Term[T]

trait MyListTerm[T]
case class ConsTerm[T](h:Term[T], tail:MyListTerm[T]) extends MyListTerm[T]
case class SentenceTerm(words:Term[MyList[String]], tags:Term[MyList[String]])

def query(s:Sentence) = s.words.count(_ == "A") > 1

def I(pred:Boolean) = if (pred) 1.0 else 0.0
//def I(pred:Term[Boolean]) = if (state(pred)) 1.0 else 0.0
def model(s:Sentence) = I(get(2,s.tags) == "V")


def simple(s:Sentence) = I(s.words(1) == "A")
//det t_I(pred:T[Boolean])(implicit state) = Term(if (pred.value) 1.0 else 0.0)
//def t_simple(s:SentenceTerm)(implicit state:State) = t_I(getTerm(s,1) t== Const("A"))


//class Factor(score: =>Double, args:List[Var])
//class Factor(score: State=>Double, args:List[Var])
//def factorModel(s:SentenceTerm) = Factor(I(getTerm(2,s.tags).value == "V"),getTerm(2,s.tags).variables)
//def factorModel(s:SentenceTerm) = Factor(state => I(state(getTerm(2,s.tags)) == "V"),getTerm(2,s.tags).variables)
//Factor(s =>
/*
 1) Search space generation Term[T]
 2) First factor graph representation expression Term[T] => Seq[Factor]
 3) push-down product


 */


def sum[T](elems: List[T])(obj: T => Double): Double = elems match {
  case Nil => 0.0
  case h :: t => obj(h) + sum(t)(obj)
}
//todo: does this work for tail recursive version?

def dot[K](keys: List[K])(arg1: K => Double, arg2: K => Double) = {
  sum(keys) { k => arg1(k) * arg2(k) }
}

val keys = List(0, 1)
val dom = List(2,3)

val w = Map(0 -> 0.5) withDefaultValue 0.0
val x = Map(2 -> true, 3 -> false)


def phi(i:Int,x:Map[Int,Boolean]) = Map(i -> I(x(i)))

dot(keys)(w,k => sum(dom)(i => phi(i,x)(k)))
sum(dom){i => dot(keys)(w, phi(i,x))}

//trait Term[T] { def vars:List[Var[_]] }
//class Var[T] extends Term[T]
//def generatedMethod(searchSpaceTerm:Term[T]):Seq[Factor]
//def goBack(state,searchSpaceTerm): T

//if (x) A + B + C  else D + E
