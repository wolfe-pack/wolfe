package ml.wolfe

import cc.factorie.la.DenseTensor1

import scala.pickling._
import scala.pickling.pickler.PrimitiveArrayPicklers

/**
 * @author riedel
 */
trait FactoriePicklers {
  implicit val denseTensor1Pickler: Pickler[DenseTensor1] with Unpickler[DenseTensor1] =
    new Pickler[DenseTensor1] with Unpickler[DenseTensor1] with PrimitiveArrayPicklers {

      implicit val arrayPickler = doubleArrayPickler


      def pickle(picklee: DenseVector, builder: PBuilder) = {
        builder.beginEntry(picklee)
        builder.putField("values",b => {
          b.hintTag(implicitly[FastTypeTag[Array[Double]]])
          b.hintStaticallyElidedType()
          arrayPickler.pickle(picklee.asArray,b)
        })
        builder.endEntry()
      }

      def tag = FastTypeTag[DenseTensor1]

      def unpickle(tag: String, reader: PReader) = {
        val reader1 = reader.readField("values")
        reader1.hintTag(implicitly[FastTypeTag[Array[Double]]])
        reader1.hintStaticallyElidedType()

        val tag = reader1.beginEntry()
        val values = arrayPickler.unpickle(tag,reader1)
        reader1.endEntry()
        new DenseVector(values.asInstanceOf[Array[Double]])
      }
    }
}

object WolfePicklers extends FactoriePicklers