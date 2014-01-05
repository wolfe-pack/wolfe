package scalapplcodefest.example

import scalapplcodefest.Wolfe


/**
 * @author Sebastian Riedel
 */
object DomainExample extends App {

  import Wolfe._

  def all[A,B](mapper:A=>B)(implicit dom:Set[A]):Set[B] = dom map mapper
  implicit def Pred[A](implicit dom:Set[A]):Set[Pred[A]] = preds(dom)
  implicit def Cross2[A1,A2](implicit dom1:Set[A1],dom2:Set[A2]):Set[(A1,A2)] = c(dom1,dom2)
  implicit def Cross3[A1,A2,A3](implicit dom1:Set[A1],dom2:Set[A2],dom3:Set[A3]):Set[(A1,A2,A3)] = c(dom1,dom2,dom3)

  case class Person(name:Symbol)
  case class Data(smokes:Pred[Person], friend:Pred[(Person,Person)])

  implicit val allPersons = Set(Person('Anna),Person('Bob))
  implicit val allData = all(Data)

  //val best = argmax (obj)

  println(allData.mkString("\n"))








}
