package ml.wolfe.apps.factorization.hack

import java.io.FileWriter

import ml.wolfe.nlp.io.{ChunkReader, CoNLLReader}
import ml.wolfe.nlp._
import ml.wolfe.util.ProgressBar

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

/**
 * @author rockt
 */
object CoNLLHackReader extends App {
  def fromCoNLLHack(lines: IndexedSeq[String]): Sentence = {
    // ID FORM LEMMA POS PPOS - DEPHEAD DEPLABEL NERLABEL
    val cells = lines.filterNot(_.isEmpty).map(_.split("\t"))
    val tokens = cells.map { c =>
      Token(c(1), CharOffsets(c(0).toInt, c(0).toInt), posTag = c(3), lemma = c(2))
    }
    val entityMentions = collectMentions(cells.map(_.apply(8))).toIndexedSeq
    val dependencies = DependencyTree(tokens, cells.zipWithIndex.map { case (c, i) => (i + 1, c(6).toInt, c(7)) })
    Sentence(tokens, syntax = SyntaxAnnotation(tree = null, dependencies = dependencies), ie = IEAnnotation(entityMentions))
  }

  def collectMentions(t: Seq[String]): Seq[EntityMention] = {
    t.zipWithIndex.foldLeft(ListBuffer[EntityMention]())((mentions: ListBuffer[EntityMention], tokenWithIndex) => {
      val (label, ix) = tokenWithIndex
      val Array(prefix, labelType) = if (label == "O") Array("O", "O") else label.split("-")

      prefix match {
        case "O" => mentions
        case "B" => mentions :+ EntityMention(labelType,ix, ix+1)
        case "I" =>
          if (mentions.isEmpty) mentions :+ EntityMention(labelType,ix, ix+1)
          else {
            val last = mentions.last
            if (last.end == ix && last.label == labelType)
              mentions.updated(mentions.length - 1, mentions.last.expandRight(1))
            else mentions :+ EntityMention(labelType,ix, ix+1)
          }
      }}).toSeq
  }

  def entityToString(entity: EntityMention, sentence: Sentence): String = sentence.tokens.slice(entity.start, entity.end).map(_.word).mkString(" ")

  //todo: debug
  def shortestPathString(e1: EntityMention, e2: EntityMention, s: Sentence): String = {
    val arcs = s.syntax.dependencies.arcs

    val from = e1.start
    val to = e2.end

    def breadthFirst(todo: List[(Int, Int, String)], acc: List[(Int, Int, String)]): List[(Int, Int, String)] = {
      if (todo.isEmpty) acc //fixme this is actually an error that should never occur
      else if (acc.last._1 == to || acc.last._2 == to) acc
      else if (acc.isEmpty) todo.filter(a => a._1 == from || a._2 == from).map(a => breadthFirst(todo.filterNot(_ == a), List(a))).minBy(_.length)
      else todo.filter(arc => acc.last._2 == arc._1 || acc.last._1 == arc._2).map(a => breadthFirst(todo.filterNot(_ == a), a :: acc)).minBy(_.length)
    }

    "path#" + breadthFirst(arcs.toList, Nil).map(a => s.tokens(a._1).word + "->" + a._3 + "->" + s.tokens(a._2).word).mkString(" ")
  }

  def writePatterns(doc: Document, writer: FileWriter): Unit = {
    //println(doc)
    for {
      s <- doc.sentences
      es = s.ie.entityMentions
      e1 <- es
      e2 <- es
      if e1 != e2
    } {
      writer.write(s"${shortestPathString(e1, e2, s)}\t${entityToString(e1, s)}\t${entityToString(e2, s)}\tTrain\t1.0\n")
    }
  }

  def apply(inputFileName: String, outputFileName: String, delim: String = "\t"): Unit = {
    val reader = new ChunkReader(inputFileName)
    val writer = new FileWriter(outputFileName)

    val progressBar = new ProgressBar(reader.iterator.length, 10000)
    progressBar.start()

    var id = ""
    var sentenceBuffer = new ArrayBuffer[Sentence]()

    reader.toStream.foreach { s =>
      val lines = s.split("\n")

      if (lines.head.startsWith("#")) {
        writePatterns(Document(sentenceBuffer.map(_.toText).mkString(" "), sentenceBuffer, Some(id)), writer)
        id = lines.head
        sentenceBuffer = new ArrayBuffer[Sentence]()
        sentenceBuffer += fromCoNLLHack(lines.filterNot(l => l.startsWith("#begin") || l.startsWith("#end") || l.isEmpty))
      } else sentenceBuffer += fromCoNLLHack(lines)

      progressBar()
    }

    writer.close()
  }


  CoNLLHackReader(args.lift(0).getOrElse("./data/bbc/Publico_prb_parsed.conll"), args.lift(1).getOrElse("./data/bbc/matrix.txt"))
}
