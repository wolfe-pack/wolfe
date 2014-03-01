package scalapplcodefest

import scalapplcodefest.sbt.Analyze

/**
 * @author Sebastian Riedel
 */
@Analyze object ShortMathDSL extends SampleSpaceDefs with StatsDefs with VectorDefs {

  def sum[T, N](over: Iterable[T])(obj: T => N)(implicit num: Numeric[N]): N =
    Wolfe.sum(over)(_ => true)(obj)(num)

//  def sum2[T, N](obj: T => N)(implicit over:Iterable[T], num: Numeric[N]): N =
//    Wolfe.sum(over)(_ => true)(obj)(num)

//  def sum2[T, N](over: Iterable[T])(pred: T => Boolean)(obj: T => N)(implicit num: Numeric[N]): N =
//    Wolfe.sum(over)(pred)(obj)(num)

  def argmax[T, N](over: Iterable[T])(pred: T => Boolean)(obj: T => N)(implicit num: Numeric[N]): T =
    Wolfe.argmax(over)(pred)(obj)(num)

  def argmax[T, N](pred: T => Boolean)(obj: T => N)(implicit over: Iterable[T], num: Numeric[N]): T =
    Wolfe.argmax(over)(pred)(obj)(num)

//  def argmax[T, N](obj: T => N)(pred: T => Boolean)(implicit over: Iterable[T], num: Numeric[N]): T =
//    Wolfe.argmax(over)(pred)(obj)(num)

  def argmax[T, N](over: Iterable[T])(obj: T => N)(implicit num: Numeric[N]): T =
    Wolfe.argmax(over)(_ => true)(obj)(num)

  def argmax[T, N](obj: T => N)(implicit over: Iterable[T], num: Numeric[N]): T =
    Wolfe.argmax(over)(_ => true)(obj)(num)

  def argmin[T, N](obj: T => N)(implicit over: Iterable[T], num: Numeric[N]): T =
    Wolfe.argmin(over)(_ => true)(obj)(num)


  def max[T, N](obj: T => N)(implicit over: Iterable[T], where: T => Boolean = (_: T) => true, num: Numeric[N]): N =
    Wolfe.max(over)(where)(obj)(num)

}
