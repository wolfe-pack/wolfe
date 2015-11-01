package ml.wolfe.compiler.torch

import breeze.linalg.DenseMatrix
import com.typesafe.scalalogging.LazyLogging
import org.zeromq.ZMQ

import scala.language.dynamics

/**
 * @author riedel
 */
object ZeroMQPlayground {

  def main(args: Array[String]) {

    val client = new TorchZeroMQClient()

    val test = client.call("torch.Tensor")(2)
    println(test)

    val test2 = client.global.torch.Tensor(2)

    println(test2)

    //could we write:
    //$.a = $.torch.Tensor(2)
    //$.b = $.a + $.b
    //$.func = function(@.x, @.y){ @.x + @.y}
    //get($.b)
    //get($.a)
    //get(torch.Tensor(2))

  }

}

class TorchZeroMQClient(port: Int = 7000) extends LazyLogging {

  import org.json4s.JsonDSL._
  import org.json4s._
  import org.json4s.jackson.JsonMethods._

  val context = ZMQ.context(1)
  logger.info("Connecting to Torch Server")

  val socket = context.socket(ZMQ.REQ)
  socket.connect(s"tcp://localhost:$port")


  def toJson(value: Any): JValue = {
    value match {
      case i: Int => JInt(i)
      case d: Double => JDouble(d)
      case t: DenseMatrix[_] =>
        val dims = if (t.cols == 1) List(t.rows) else List(t.rows, t.cols) //todo: currently pretending n x 1 matrices are vectors
        ("_datatype" -> "tensor") ~
          ("dims" -> dims) ~
          ("storage" -> t.toArray.toList.asInstanceOf[List[Double]])
      case p: Product =>
        val args = p.productIterator.map(toJson).toList
        JArray(args)
      case _ => JString(value.toString)
    }
  }

  def fromJson(value: JValue): Any = {
    implicit val formats = DefaultFormats
    value match {
      case JInt(i) => i
      case JDouble(d) => d
      case obj: JObject if obj \ "_datatype" == JString("tensor") =>
        val dims = (obj \ "dims").extract[List[Int]]
        val storage = (obj \ "storage").extract[List[Double]]
        dims match {
          case List(rows, cols) =>
            new DenseMatrix[Double](rows, cols, storage.toArray)
          case List(rows) =>
            new DenseMatrix[Double](rows, 1, storage.toArray)
        }
      case JArray(args) =>
        args.map(fromJson)
      case _ => value
    }
  }

  def call(function: String)(args: Any*): Any = {
    call(function.split("\\.").toList)(args.toList)
  }

  def call(function: List[String])(args: List[Any]): Any = {
    val json =
      ("cmd" -> "call") ~
        ("msg" -> (
          ("func" -> function) ~
            ("args" -> args.map(toJson))
          ))

    val jsonString = compact(render(json))

    logger.info(s"Calling Lua function: ${function.mkString(".")}")

    socket.send(jsonString)

    val msg = socket.recvStr()

    val jsonMsg = parse(msg)
    val decoded = fromJson(jsonMsg)
    decoded
  }

  val global = new Module(Nil)

  class Module(prefix: List[String]) extends Dynamic {

    def selectDynamic(name: String) = new Module(prefix :+ name)

    def applyDynamic(name: String)(args: Any*) = {
      call(prefix :+ name)(args.toList)
    }
  }


}
