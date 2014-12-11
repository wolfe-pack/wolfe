package ml.wolfe.apps.factorization.hack

import java.io.FileWriter
import java.util
import java.util.Stack

import cc.factorie.app.nlp.ner.NerTag
import cc.factorie.app.nlp.parse.ParseTree
import ml.wolfe.nlp.io.{ChunkReader, CoNLLReader}
import ml.wolfe.nlp._
import ml.wolfe.util.ProgressBar

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source

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

  def entityToString(entity: EntityMention, sentence: Sentence): String =
    sentence.tokens.slice(entity.start, entity.end).map(_.word).mkString(" ") + "#" + entity.label

  //todo: debug
  def shortestPathString(e1: EntityMention, e2: EntityMention, s: Sentence): String = {
    /*
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
    */
    "path"
  }

  def writePatterns(doc: Document, writer: FileWriter): Unit = {
    //println(doc)
    for {
      s <- doc.sentences
      es = s.ie.entityMentions
      e1 <- es
      e2 <- es
      if e1 != e2 && e1.start < e2.start
    } {
      writer.write(s"${shortestPathString(e1, e2, s)}\t${entityToString(e1, s)}\t${entityToString(e2, s)}\tTrain\t1.0\n")
    }
  }

  def apply(inputFileName: String, outputFileName: String, delim: String = "\t"): Unit = {
    val linesIterator = Source.fromFile(inputFileName).getLines().buffered
    val writer = new FileWriter(outputFileName)

    println("Estimating completion time...")
    val progressBar = new ProgressBar(Source.fromFile(inputFileName).getLines().size, 100000)
    progressBar.start()


    println("Start processing...")
    var id = ""
    var sentenceBuffer = new ArrayBuffer[Sentence]()
    var linesBuffer = new ArrayBuffer[String]()

    linesIterator.toStream.foreach { l =>
      if (l.isEmpty) {
        sentenceBuffer += fromCoNLLHack(linesBuffer)
        linesBuffer = new ArrayBuffer[String]()
      } else if (l.startsWith("#")) {
        writePatterns(Document(sentenceBuffer.map(_.toText).mkString(" "), sentenceBuffer, Some(id)), writer)
        id = l
        sentenceBuffer = new ArrayBuffer[Sentence]()
        linesBuffer = new ArrayBuffer[String]()
        sentenceBuffer += fromCoNLLHack(linesBuffer.filterNot(l => l.startsWith("#begin") || l.startsWith("#end") || l.isEmpty))
      } else {
        //sentenceBuffer += fromCoNLLHack(linesBuffer)
        linesBuffer += l
      }

      progressBar()
    }

    println("Done!")

    writer.close()
  }


  CoNLLHackReader(args.lift(0).getOrElse("./data/bbc/Publico_prb_parsed.conll"), args.lift(1).getOrElse("./data/bbc/matrix_publico.txt"))
  //CoNLLHackReader(args.lift(0).getOrElse("./data/bbc/BBC_ner_parsed.conll"), args.lift(1).getOrElse("./data/bbc/matrix_bbc.txt"))
}


/*
/**
 * @author lmyao
 * @author rockt
 */
object DepUtils {
  val FEASIBLEPOS = Set("NN", "NNP", "NNS", "NNPS", "VB", "VBD", "VBZ", "VBG", "VBN", "VBP", "RB", "RBR", "RBS", "IN", "TO", "JJ", "JJR", "JJS", "PRP", "PRP$")
  val JUNKLABELS = Set("conj", "ccomp", "parataxis", "xcomp", "pcomp", "advcl", "punct", "infmod", "cop", "abbrev", "neg", "det", "prt")     //I remove 'rcmod', Limin Yao , path staring with adv should be excluded
  val CONTENTPOS = Set("NN", "NNP", "NNS", "NNPS", "VB", "VBD", "VBZ", "VBG", "VBN", "VBP", "PRP", "PRP$", "RB", "RBR", "RBS", "JJ", "JJR", "JJS")
  val JUNKROOT = Set("say", "describe", "tell")
  val SUFFIX = Set("Jr.", "II", "Jr", "d.l.", "D.L.", "d.l", "D.L")

  // find all the ancestors of a node in order to find common ancestor of two nodes, called by find_path
  def findAncestors(ancestors: util.Stack[Int], id: Int, depTree: ParseTree) {

    //due to parsing error, there are may be more than one roots, todo:
    //val rootEdge = depTree.modifiers(sentence.root).first

    var cur = id
    ancestors.push(cur)
    while (depTree.parentIndex(cur) != -1) {
      //sentence.root's index is -1
      cur = depTree.parentIndex(cur)
      ancestors.push(cur)
    }
  }


  //find the path from a source node to a dest node and the relationship, denoted by ->-><-<-  source != dest
  //return the rootOfPath and PathString
  def findPath(source: Int, dest: Int, depTree: ParseTree): (String, String, Seq[(Token, String)]) = {
    val tokens = depTree.sentence.tokens
    val srctmp = new util.Stack[Int] //source queue
    val srcStack = new util.Stack[Int]
    val destStack = new util.Stack[Int]
    val path = new ArrayBuffer[(Token, String)]
    findAncestors(destStack, dest, depTree)
    findAncestors(srcStack, source, depTree)

    var junk = false // for indicating junk path
    var res = ""
    //find common ancestor
    var common_ancestor: Int = -1
    while (!srcStack.empty && !destStack.empty && srcStack.peek == destStack.peek) {
      common_ancestor = srcStack.peek
      srcStack.pop
      destStack.pop
    }

    if (common_ancestor == -1) {
      path += ((depTree.rootChild, "exception"))
      return ("null", "exception", path)
    }

    //reverse the order of nodes in srcStack by a tmp stack
    while (!srcStack.empty) {
      val top = srcStack.pop
      srctmp.push(top)
    }

    //get nodes from srctmp, destStack and fill in path, todo:without source and dest node, find lemmas for each word using stanford pipeline annotator
    while (!srctmp.empty) {
      val top = srctmp.pop
      if (!FEASIBLEPOS.contains(tokens(top).attr[PosTag].value)) junk = true
      val label = depTree.label(top).categoryValue.toLowerCase
      if (JUNKLABELS.contains(label)) junk = true
      path += ((tokens(top), label))
      if (top != source) {
        if (tokens(top).attr[NerTag].value != "O") res += tokens(top).attr[NerTag].value + "<-" + label + "<-"
        else if(SUFFIX.contains(tokens(top).string)) res += tokens(top).attr[NerTag].value + "<-" + label + "<-"
        else  res += tokens(top).attr[Lemma].value + "<-" + label + "<-"
      }
      else
        res += "<-" + label + "<-"
    }
    path += ((tokens(common_ancestor), "rootOfPath")) //this is common ancestor of source and dest , // tokens(top).lemma, todo
    if(JUNKROOT.contains(tokens(common_ancestor).attr[Lemma].value.toLowerCase))  {junk = true;     }
    if (common_ancestor != source && common_ancestor != dest) {
      if (tokens(common_ancestor).attr[NerTag].value != "O") res += tokens(common_ancestor).attr[NerTag].value
      else if(SUFFIX.contains(tokens(common_ancestor).string)) res += tokens(common_ancestor).attr[NerTag].value
      else res += tokens(common_ancestor).attr[Lemma].value
    }
    while (!destStack.empty) {
      val top = destStack.pop
      if (!FEASIBLEPOS.contains(tokens(top).attr[PosTag].value)) {junk = true;  }
      val label = depTree.label(top).categoryValue.toLowerCase
      if (JUNKLABELS.contains(label)) {junk = true;    }
      path += ((tokens(top), label))
      if (top != dest) {
        //if (tokens(top).attr[NerTag].value != "O") junk = true //named entity should not appear on the path
        //if (SUFFIX.contains(tokens(top).string)) junk = true //suffix should not appear on the path
        if (tokens(top).attr[NerTag].value != "O") res += "->" + label + "->" + tokens(top).attr[NerTag].value
        else if(SUFFIX.contains(tokens(top).string)) res += "->" + label + "->" + tokens(top).attr[NerTag].value
        else res += "->" + label + "->" + tokens(top).attr[Lemma].value

      }
      else
        res += "->" + label + "->"
    }


    var content = false //path includes source and dest tokens, there should be content words between them
    val numContentWords = path.map(_._1).map(_.attr[PosTag].value).filter(CONTENTPOS.contains(_)).size

    if (numContentWords > 1) content = true // else {println("Junk content!")}
    if (!junk && !content) junk = true //heuristics for freebase candidate generation

    if(path.map(_._1).map(_.string).filter(_.matches("[\\?\"\\[]")).size > 0) junk = true

    if(path(0)._2 == "adv") junk = true
    if(path(0)._2 == "name" && path.reverse(0)._2 == "name") junk = true
    if(path(0)._2 == "sbj" && path.last._2 == "sbj") junk = true

    //filter out junk according to DIRT, rule 2, any dependency relation must connect two content words
    if (junk) {
      path += ((depTree.rootChild, "junk"))
      return ("null", "junk", path)
    } //comment this out for generating freebase instances

    (tokens(common_ancestor).attr[Lemma].value.toLowerCase, res.toLowerCase, path)
  }

  //find out mention of the SF entity in the sentence, output a mention
  def findOneArg(entstr : String, slot: Mention, sentence : Sentence) : Mention = {
    val words = entstr.split("[-\\s]")
    val abbr = words.map(_.substring(0,1)).mkString("")
    val candidates = new ArrayBuffer[Mention]
    val depTree = sentence.attr[ParseTree]
    for(token <- sentence.tokens.filter(_.attr[NerTag].value != "O")){
      if(entstr.contains(token.string) || (abbr.matches("^[A-Z]+$") && token.string.compareTo(abbr) == 0 ) ){  //second condition for handling abbr.
      var ancestor = token.sentencePosition
        while(depTree.parentIndex (ancestor) != -1  &&
        entstr.contains(sentence.tokens(depTree.parentIndex(ancestor)).string)
        )
        {
          ancestor = depTree.parentIndex(ancestor)
        }
        var tokenEnd = sentence.tokens(ancestor).position
        var tokenStart = token.position
        if(tokenEnd < tokenStart){
          val tmp = tokenEnd
          tokenEnd = tokenStart
          tokenStart = tmp
        }
        val mention = new Mention
        mention.docId := sentence.document.name
        mention.tokenBegin := tokenStart
        mention.tokenEnd := tokenEnd
        mention.charBegin := sentence.document.tokens(tokenStart).stringStart
        mention.charEnd := sentence.document.tokens(tokenEnd).stringEnd

        //mention.phrase := sentence.document.string.slice(mention.charBegin.value, mention.charEnd())
        mention.phrase := entstr
        candidates += mention
      }
    }

    val slotHead = EntityMentionUtils.headOfMention(slot,sentence)
    var maxDis = sentence.tokens.size
    var res : Mention = null
    for(mention <- candidates){
      val dist = math.abs(EntityMentionUtils.headOfMention(mention,sentence)-slotHead)
      if(dist < maxDis){
        maxDis = dist
        res = mention
      }
    }

    //    //for debugging:
    //    if(res != null)
    //      println(entstr + "\t" + slot.phrase() + "\t" + res.phrase() )

    //    if(res == null){
    //      for(token <- sentence.tokens.filter(x => (x.attr[PosTag].value == "PRP$" || x.attr[PosTag].value == "PRP")) ) {//still some errors here, it can only be linked to non-person
    //        val mention = new Mention
    //        mention.charBegin := token.stringStart
    //        mention.charEnd := token.stringEnd
    //        mention.tokenBegin := token.position
    //        mention.tokenEnd := token.position
    //        //mention.phrase := token.string
    //        mention.phrase := entstr
    //        candidates += mention
    //      }
    //    }
    //    maxDis = sentence.tokens.size
    //    for(mention <- candidates){
    //      val dist = math.abs(EntityMentionUtils.headOfMention(mention,sentence)-slotHead)
    //      if(dist < maxDis){
    //        maxDis = dist
    //        res = mention
    //      }
    //    }
    return res
  }

  def findOneArg(entstr : String, sentence : Sentence) : Seq[Mention] = {
    val words = entstr.split("[-\\s]")
    val abbr = words.map(_.substring(0,1)).mkString("")
    val candidates = new ArrayBuffer[Mention]
    val depTree = sentence.attr[ParseTree]
    for(token <- sentence.tokens.filter(_.attr[NerTag].value != "O")){
      if(entstr.contains(token.string) || (abbr.matches("^[A-Z]+$") && token.string.compareTo(abbr) == 0 ) ){  //second condition for handling abbr.
      var ancestor = token.sentencePosition
        while(depTree.parentIndex (ancestor) != -1  &&
        entstr.contains(sentence.tokens(depTree.parentIndex(ancestor)).string)
        )
        {
          ancestor = depTree.parentIndex(ancestor)
        }
        var tokenEnd = sentence.tokens(ancestor).position
        var tokenStart = token.position
        if(tokenEnd < tokenStart){
          val tmp = tokenEnd
          tokenEnd = tokenStart
          tokenStart = tmp
        }
        val mention = new Mention
        mention.docId := sentence.document.name
        mention.tokenBegin := tokenStart
        mention.tokenEnd := tokenEnd
        mention.charBegin := sentence.document.tokens(tokenStart).stringStart
        mention.charEnd := sentence.document.tokens(tokenEnd).stringEnd

        mention.phrase := sentence.document.string.slice(mention.charBegin(), mention.charEnd())
        mention.canonical := entstr
        candidates += mention
      }
    }

    return candidates
  }
}
*/

