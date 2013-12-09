package scalapplcodefest.value

/**
 * @author Sebastian Riedel
 */
case class CartesianProduct2[A1,A2](d1:Set[A1],d2:Set[A2]) extends SetValue[(A1,A2)] {
  def contains(elem: (A1, A2)) = d1(elem._1) && d2(elem._2)
  def iterator = for (v1 <- d1.iterator; v2 <- d2.iterator) yield (v1,v2)
}
case class CartesianProduct3[A1,A2,A3](d1:Set[A1],d2:Set[A2],d3:Set[A3]) extends SetValue[(A1,A2,A3)] {
  def contains(elem: (A1, A2, A3)) = d1(elem._1) && d2(elem._2) && d3(elem._3)
  def iterator = for (v1 <- d1.iterator; v2 <- d2.iterator; v3 <- d3.iterator) yield (v1,v2,v3)
}



case object MapIterable extends PartialFunction[(Iterable[Any],Fun[Any,Any]),Iterable[Any]] {
  def apply(v1: (Iterable[Any], Fun[Any, Any])) = v1 match {case (s,f) => s.map(f)}
  def isDefinedAt(x: (Iterable[Any], Fun[Any, Any])) = true
}

case object CollectIterable extends PartialFunction[(Iterable[Any],Fun[Any,Any]),Iterable[Any]] {
  def apply(v1: (Iterable[Any], Fun[Any, Any])) = v1 match {case (s,f) => s.collect(f)}
  def isDefinedAt(x: (Iterable[Any], Fun[Any, Any])) = true
}

case object FilterIterable extends PartialFunction[(Iterable[Any],Fun[Any,Boolean]),Iterable[Any]] {
  def apply(v1: (Iterable[Any], Fun[Any, Boolean])) = v1 match {case (s,f) => s.filter(f)}
  def isDefinedAt(x: (Iterable[Any], Fun[Any, Boolean])) = true
}

case object IsDefined extends PartialFunction[(PartialFunction[Any,Any],Any),Boolean] {
  def isDefinedAt(x: (PartialFunction[Any, Any], Any)) = true
  def apply(x: (PartialFunction[Any, Any], Any)) = x match {case (f,a) => f.isDefinedAt(a)}
}


