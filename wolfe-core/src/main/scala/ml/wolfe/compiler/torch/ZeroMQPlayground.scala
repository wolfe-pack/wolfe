package ml.wolfe.compiler.torch

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.json4s.jackson.JsonMethods._
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

class TorchZeroMQClient(port:Int = 7000) extends LazyLogging {

  import org.json4s._
  import org.json4s.JsonDSL._
  import org.json4s.jackson.JsonMethods._

  val context = ZMQ.context(1)
  logger.info("Connecting to Torch Server")

  val socket = context.socket(ZMQ.REQ)
  socket.connect(s"tcp://localhost:$port")


  def toJson(value:Any) = {
    value match {
      case i:Int => JInt(i)
      case d:Double => JDouble(d)
      case _ => JString(value.toString)
    }
  }

  def call(function:String)(args:Any*):Any = {
    call(function.split("\\.").toList)(args.toList)
  }

  def call(function:List[String])(args:List[Any]):Any = {
    val json =
      ("cmd" -> "call") ~
        ("msg" -> (
          ("func" -> function) ~
            ("args" -> args.map(toJson))
          ))

    val jsonString = compact(render(json))

    socket.send(jsonString)

    val msg = socket.recvStr()
    msg
  }

  val global = new Module(Nil)

  class Module(prefix:List[String]) extends Dynamic {

    def selectDynamic(name: String) = new Module(prefix :+ name)

    def applyDynamic(name: String)(args: Any*) = {
      call(prefix :+ name)(args.toList)
    }
  }


}
