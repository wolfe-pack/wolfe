package ml.wolfe.nlp.io

import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 8/1/14.
 */

abstract class ProtoNode {
  def search: Seq[ProtoNode]
}

case class ProtoValueNode(field: String, value: String) extends ProtoNode {

  def search: Seq[ProtoNode] = Seq(this)
}

case class ProtoParentNode(field: String, value: Seq[ProtoNode]) extends ProtoNode {
  def search: Seq[ProtoNode] = Seq(this) ++ value.map(_.search).flatten
}


class ProtoReader(ldelim: String = "(", rdelim: String = ")") {
  val VALUE_PATTERN = """ *([^:]+): (.*)""".r
  val PARENT_PATTERN = """ *([^ ]+) \{""".r

  def parse(text: String): Seq[ProtoNode] = {
    var lines = text.split("\n")
    val nodes = new ArrayBuffer[ProtoNode]
    while (lines.nonEmpty) {
      lines.head match {
        case VALUE_PATTERN(field, value) => {
          nodes += ProtoValueNode(field, value)
          lines = lines.tail
        }
        case PARENT_PATTERN(field) => {
          val name = lines.head.split("\\{").head.trim
          val inner = lookAhead(lines)
          nodes += ProtoParentNode(name, parse(inner.mkString("\n")))
          lines = lines.slice(inner.size+1, lines.size)
        }
        case _ => lines = lines.tail
      }
    }
    nodes.toSeq
  }

  def lookAhead(lines: Array[String]): Array[String] = {
    var count = 1
    var i = 1
    while (i < lines.size && count > 0) {
      if (lines(i).contains("{")) count += 1
      if (lines(i).contains("}")) count -= 1
      i += 1
    }
    lines.slice(1, i - 1)
  }
}

object ProtoReader {

  def main(args: Array[String]): Unit = {
    val sreader = new ProtoReader(ldelim = "{", rdelim = "}")
    val str =
      """test {
           f1: v1
           f2: v2
           inner {
             if1: iv1
             deeper {
              df1: dv1
             }
           }
         }
      """.stripMargin
    println("Begin...")
    sreader.parse(str).foreach { n =>
      println(n)
    }
    println("\nNodes:")
    println(sreader.parse(str).head.search.mkString("\n"))
  }
}









//
//  def readfromFile(filename: String): Iterator[String] = {
//    val text = scala.io.Source.fromFile(filename).getLines().mkString(" ")
//    Iterator.continually(readNext(text)).takeWhile(_ != None)
//  }
//
//  def readfromFile[T](filename: String, parseString: String => T): Iterator[T] = {
//    val text = scala.io.Source.fromFile(filename).getLines().mkString(" ")
//    Iterator.continually(parseString(readNext(text))).takeWhile(_ != null)
//  }
//
//  def readFromString(str: String): Iterator[ProtoNode] = {
//    Iterator.continually(parseSExp(str)).takeWhile(_ != null)
//  }




//  def parseSExp(text: String): SExpNode = {
//    println(text.replaceAll("\n", " "))
//    var stack = List[Int]()
//    var level = 0
//    var processed = 0
//    var ntext = ""
//    var children = new ArrayBuffer[SExpNode]
//    while (processed < text.size) {
//      println(processed)
//      val letter = text.substring(processed, processed+1)
//      if (letter == ldelim) {
//        level += 1
//        if (processed == 0) {
//          println("starting")
//        }
//        else {
//          val child = parseSExp(text.substring(processed))
//          println("child: " + child.text)
//          children += child
//         // stack = processed +: stack
//         // level += 1
//        }
//      }
//      else if (letter == rdelim) {
//        SExpNode(ntext, children.toList)
//      }
//      processed += 1
//    }
//    null // SExpNode(ntext, children)
//  }

//  def parseSExp(text: String): SExpNode = {
//    var count = 0
//    println("size = " + text.size)
//    println("c: " + count)
//    val start = processed
//    if (start >= text.size)
//      return null
//    var letter = text.substring(processed, processed+1)
//    while (processed < text.size) {
//      println(processed + " : " + count)
//      processed += 1
//      letter = text.substring(processed-1, processed)
//      if (letter == ldelim) {
//        count += 1
//      }
//      else if (letter == rdelim) {
//        count -= 1
//        if (count == 0) {
//          return text.substring(start, processed).trim
//        }
//      }
//    }
//    null
//  }


// case clas