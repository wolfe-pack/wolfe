package ml.wolfe.ui

/**
 * Created by luke on 16/04/15.
 */
object Json {
  case class Json(str:String)

  val indentStr: (Int, String) => String = (n:Int, str:String) =>
    str.replace("\n", "\n" + " " * n)

  private def toJson(x:Any, indent: (Int, String) => String ):Json = x match {
      case j:Json       => j
      case s:String     => Json("\"" + s + "\"")
      case n:java.lang.Number => Json(n.toString)
      case b:Boolean    => Json(b.toString)

      case m:Map[_, _]  => Json("{" + indent(1,{
        val seq = m.seq.map{case (k, v) =>
          val key = toJson(k, indent).str
          key + ": " + indent(key.length + 2, toJson(v, indent).str)
        }

        if(seq.map(_.length).sum < 60 && !seq.exists(_.contains("\n")))
          seq.reduce(_ + ", " + _)
        else
          seq.reduce(_ + ",\n" + _)
      }) + "}")

      case s:Seq[_]     => Json("[" + indent(1, {
        val seq = s.map(x => toJson(x, indent).str)
        if(seq.map(_.length).sum < 60 && !seq.exists(_.contains("\n")))
          seq.reduce(_ + ", " + _)
        else
          seq.reduce(_ + ",\n" + _)
      }) + "]")
    }

  def toJson(x:Any, indent:Boolean = false):Json = {
    if(indent) toJson(x, indentStr)
    else toJson(x, {(n, s) => s})
  }
}
