package ml.wolfe.compiler.torch

import org.zeromq.ZMQ

/**
 * @author riedel
 */
object ZeroMQPlayground {


  import org.json4s._
  import org.json4s.JsonDSL._
  import org.json4s.jackson.JsonMethods._

  def main(args: Array[String]) {
    val context = ZMQ.context(1)

    println("Connecting to Torch Server")

    val socket = context.socket(ZMQ.REQ)

    socket.connect("tcp://localhost:7000")

    val json =
      ("cmd" -> "call") ~
        ("msg" -> (
          ("func" -> List("torch", "Tensor")) ~
            ("args" -> 3)
          ))

    val jsonString = compact(render(json))

    socket.send(jsonString)

    val msg = socket.recvStr()
    println("Received: " + msg)


  }

}
