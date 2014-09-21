package ml.wolfe.mv

/**
 * @author Sebastian Riedel
 */
object AAAITest {

  import SearchSpace._
  import Transformer._


  trait MyList[+A]
  case class MyCons[+A](fst:A,snd:MyList[A]) extends MyList[A]
  case object MyNil extends MyList[Nothing]
  type MyMap[A,B] = MyList[(A,B)]



  //val test = RTCase2(MyCons.apply,RTConst(1),RTConst(List(2,3)))

  def maps[A,B](dom:List[A])(range:List[B]):List[MyMap[A,B]] = {
    if (dom.isEmpty) MyNil :: Nil else
      crossADT[(A,B),MyMap[A,B],MyCons[(A,B)]](MyCons.apply)(cross(unit(dom.head),range),maps(dom.tail)(range))
  }

  def bools = List(false,true)
  def persons = List('Ann, 'Bob)

  def s1 = maps(persons)(bools)

  val (stree,element) = transformModel(s1)(x => 1.0)




}
