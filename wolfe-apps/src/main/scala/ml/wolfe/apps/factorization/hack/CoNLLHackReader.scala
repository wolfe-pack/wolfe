package ml.wolfe.apps.factorization.hack

import java.io.FileWriter
import java.util
import java.util.Stack

import cc.factorie.app.nlp.ner.NerTag
import cc.factorie.app.nlp.parse.ParseTree
import ml.wolfe.nlp.io.{ChunkReader, CoNLLReader}
import ml.wolfe.nlp._
import ml.wolfe.util.ProgressBar
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

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
    sentence.tokens.slice(entity.start, entity.end).map(_.word).mkString(" ")

  def writePatterns(doc: Document, writer: FileWriter, english: Boolean): Unit = {
    //println("writing pattern for " + doc.filename.get)
    for {
      s <- doc.sentences
      //tmp = { println(s.toText) }
      es = s.ie.entityMentions
      e1 <- es
      e2 <- es
      if e1 != e2 //&& e1.start < e2.start
    } {
      val e1String = entityToString(e1, s)
      val e2String = entityToString(e2, s)

      def toCanonical(name: String): String = {
        val normOpt =
          if (english) EntityHackNormalization.normalizeEnglish(name)
          else EntityHackNormalization.normalizePortuguese(name)

        //if (normOpt.isDefined) println(name + "\t" + normOpt.get)

        normOpt.map(_._1).getOrElse(name)
      }

      val normE1 = toCanonical(e1String + "#" + e1.label)
      val normE2 = toCanonical(e2String + "#" + e2.label)

      val (rootOfPath, pathString, path) = DepUtils.findPath(DepUtils.headOfMention(e1, s), DepUtils.headOfMention(e2, s), s, english)

      //println(pathString)

      if (!pathString.startsWith("[EXCEPTION") && pathString != "junk" && path.size <= 5) {
        val outputLine = s"path#|$pathString|\t$normE1\t$normE2\tTrain\t1.0\n"

        //println(s.toText)
        //print(outputLine)

        writer.write(outputLine)
      }
    }
  }

  def apply(inputFileName: String, writer: FileWriter, english: Boolean = true, delim: String = "\t"): Unit = {
    val linesIterator = Source.fromFile(inputFileName).getLines().buffered

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
        writePatterns(Document(sentenceBuffer.map(_.toText).mkString(" "), sentenceBuffer, Some(id)), writer, english)
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

  }


  val writer = new FileWriter(args.lift(1).getOrElse("./data/bbc/matrix.txt"))

  CoNLLHackReader(args.lift(0).getOrElse("./data/bbc/bbc_latest.conll"), writer, english = true)
  CoNLLHackReader(args.lift(0).getOrElse("./data/bbc/bbc.conll"), writer, english = true)

  //fixme
  //CoNLLHackReader(args.lift(0).getOrElse("./data/bbc/publico.conll"), writer, english = false)

  writer.close()
}



/**
 * @author lmyao
 * @author rockt
 */
object DepUtils {
  val FEASIBLEPOS = Set("NN", "NNP", "NNS", "NNPS", "VB", "VBD", "VBZ", "VBG", "VBN", "VBP", "RB", "RBR", "RBS", "IN", "TO", "JJ", "JJR", "JJS", "PRP", "PRP$")
  //I remove 'rcmod', Limin Yao , path staring with adv should be excluded
  val JUNKLABELS = Set("conj", "ccomp", "parataxis", "xcomp", "pcomp", "advcl", "punct", "infmod", "cop", "abbrev", "neg", "det", "prt")
  val CONTENTPOS = Set("NN", "NNP", "NNS", "NNPS", "VB", "VBD", "VBZ", "VBG", "VBN", "VBP", "PRP", "PRP$", "RB", "RBR", "RBS", "JJ", "JJR", "JJS")
  val JUNKROOT = Set("say", "describe", "tell")
  //rockt: not sure what these suffixes are used for
  val SUFFIX = Set("Jr.", "II", "Jr", "d.l.", "D.L.", "d.l", "D.L")

  
  def parentIndex(id: Int, depTree: DependencyTree): Int =
    //depTree.arcs.find(a => a._2 == id).map(_._1).getOrElse(-1)
    depTree.headOf(id).getOrElse(-1)

  //rockt: unsure about the semantics of this method
  def getLabel(id: Int, depTree: DependencyTree): String =
    depTree.arcs.find(a => a._1 == id).map(_._3).getOrElse(s"[EXCEPTION: no label found for $id]")

  def getNERLabel(id: Int, s: Sentence): String =
    s.ie.entityMentions.find(e => e.start <= id && id <= e.end).map(_.label).getOrElse("O")
    
  // find all the ancestors of a node in order to find common ancestor of two nodes, called by find_path
  def findAncestors(ancestors: util.Stack[Int], id: Int, depTree: DependencyTree) {
    //due to parsing error, there are may be more than one roots, todo:
    //val rootEdge = depTree.modifiers(sentence.root).first

    var current = id
    ancestors.push(current)

    while (parentIndex(current, depTree) != -1) {
      //sentence.root's index is -1
      current = parentIndex(current, depTree)
      ancestors.push(current)
    }
  }


  //find the path from a source node to a dest node and the relationship, denoted by ->-><-<-  source != dest
  //return the rootOfPath and PathString
  def findPath(source: Int, dest: Int, sentence: Sentence, english: Boolean): (String, String, Seq[(Token, String)]) = {
    val depTree = sentence.syntax.dependencies
    val tokens = sentence.tokens
    def getToken(ix: Int) =
      if (ix == 0) Token("DEP-ROOT-DUMMY", CharOffsets(-1, -1), "DEP-ROOT-DUMMY", "DEP-ROOT-DUMMY")
      else tokens(ix - 1)

    val srctmp = new util.Stack[Int] //source queue
    val srcStack = new util.Stack[Int]
    val destStack = new util.Stack[Int]
    val path = new ArrayBuffer[(Token, String)]

    findAncestors(destStack, dest, depTree)
    findAncestors(srcStack, source, depTree)

    var junk = false // for indicating junk path
    var res = ""
    //find common ancestor
    var commonAncestor: Int = -1
    while (!srcStack.empty && !destStack.empty && srcStack.peek == destStack.peek) {
      commonAncestor = srcStack.peek
      srcStack.pop
      destStack.pop
    }

    if (commonAncestor == -1) {
      //fixme: should use root token instead; depTree.rootChild
      val exception = s"[EXCEPTION: no common ancestor found between $source and $dest]"
      path += tokens.head -> exception
      return ("null", exception, path)
    }

    //reverse the order of nodes in srcStack by a tmp stack
    while (!srcStack.empty) {
      val top = srcStack.pop
      srctmp.push(top)
    }

    //println("Tokens:\n" + sentence.tokens.mkString("\n"))

    //get nodes from srctmp, destStack and fill in path, todo:without source and dest node, find lemmas for each word using stanford pipeline annotator
    while (!srctmp.empty) {
      val top = srctmp.pop
      if (english && !FEASIBLEPOS.contains(getToken(top).posTag)) junk = true

      val label = getLabel(top, depTree).toLowerCase
      if (english && JUNKLABELS.contains(label)) junk = true

      path += getToken(top) -> label
      
      if (top != source) {
        //println("# " + top + "\t" + getNERLabel(top, sentence))
        if (getNERLabel(top, sentence) != "O")
          res += getNERLabel(top, sentence) + "<-" + label + "<-"
        else if (english && SUFFIX.contains(getNERLabel(top, sentence)))
          res += getNERLabel(top, sentence) + "<-" + label + "<-"
        else {
          res += getToken(top).lemma + "<-" + label + "<-"
        }
      }
      else
        res += "<-" + label + "<-"
    }

    //this is common ancestor of source and dest , // tokens(top).lemma, todo
    path += getToken(commonAncestor) -> "rootOfPath"


    if (english && JUNKROOT.contains(getToken(commonAncestor).lemma.toLowerCase))
      junk = true

    if (commonAncestor != source && commonAncestor != dest) {
      if (getNERLabel(commonAncestor, sentence) != "O") res += getNERLabel(commonAncestor, sentence)
      else if (english && SUFFIX.contains(getNERLabel(commonAncestor, sentence))) res += getNERLabel(commonAncestor, sentence)
      else res += getToken(commonAncestor).lemma
    }

    while (!destStack.empty) {
      val top = destStack.pop
      if (english && !FEASIBLEPOS.contains(getToken(top).posTag))
        junk = true

      val label = getLabel(top, depTree).toLowerCase
      if (english && JUNKLABELS.contains(label))
        junk = true

      path += getToken(top) -> label

      if (top != dest) {
        //if (tokens(top).attr[NerTag].value != "O") junk = true //named entity should not appear on the path
        //if (SUFFIX.contains(tokens(top).string)) junk = true //suffix should not appear on the path
        if (getNERLabel(top, sentence) != "O") res += "->" + label + "->" + getNERLabel(top, sentence)
        else if (english && SUFFIX.contains(getNERLabel(top, sentence))) res += "->" + label + "->" + getNERLabel(top, sentence)
        else res += "->" + label + "->" + getToken(top).lemma

      }
      else
        res += "->" + label + "->"
    }


    var content = false //path includes source and dest tokens, there should be content words between them
    val numContentWords = path.map(_._1).map(_.posTag).count(CONTENTPOS.contains)



    //println(path.map(_._1).map(_.posTag).filter(CONTENTPOS.contains))
    //rockt: increased this to 2a
    if (numContentWords > 2 || !english) content = true // else {println("Junk content!")}
    //if (numContentWords > 1) content = true // else {println("Junk content!")}


    if (english) {
      //rockt: we only care about paths with content
      if (!content) junk = true //heuristics for freebase candidate generation

      if (path.map(_._1).map(_.word).exists(_.matches("[\\?\"\\[]")))
        junk = true
      if (path(0)._2 == "adv")
        junk = true
      if (path(0)._2 == "name" && path.reverse(0)._2 == "name")
        junk = true
      if (path(0)._2 == "sbj" && path.last._2 == "sbj")
        junk = true

      //filter out junk according to DIRT, rule 2, any dependency relation must connect two content words
      if (junk) {
        //fixme: should use root token instead; depTree.rootChild
        path += tokens.head -> "junk"
        return ("null", "junk", path)
      } //comment this out for generating freebase instances
    }

    (getToken(commonAncestor).lemma.toLowerCase, res.toLowerCase, path)
  }


  def headOfMention(mention: EntityMention, sentence: Sentence): Int = {
    // find out the head of this mention
    val depTree = sentence.syntax.dependencies
    val beginTokenPosition = mention.start
    val untilTokenPosition = mention.end + 1

    var head = 0
    if (untilTokenPosition - beginTokenPosition == 1) head = beginTokenPosition

    else {
      //find each token's ancestors which are in the entitymention, choose the rightmost one
      head = beginTokenPosition

      for (i <- beginTokenPosition until untilTokenPosition) {
        var ancestor = i
        while (parentIndex(ancestor, depTree) < untilTokenPosition && parentIndex(ancestor, depTree) >= beginTokenPosition) {
          //println(s"$mention $beginTokenPosition $untilTokenPosition $head $ancestor ${parentIndex(ancestor, depTree)}")
          ancestor = parentIndex(ancestor, depTree)
        }
        if (ancestor > head) head = ancestor
      }
    }

    head
  }
}

object DepUtilsSpec extends App {
  val deps = Seq(
    Token("For",CharOffsets(1,1),"IN","for") -> (1,9,"ADV"),
    Token("more",CharOffsets(2,2),"JJR","more") -> (2,4,"DEP"),
    Token("than",CharOffsets(3,3),"IN","than") -> (3,4,"DEP"),
    Token("35",CharOffsets(4,4),"CD","35") -> (4,5,"NMOD"),
    Token("years",CharOffsets(5,5),"NNS","years") -> (5,1,"PMOD"),
    Token(",",CharOffsets(6,6),",",",") -> (6,9,"P"),
    Token("Ronnie",CharOffsets(7,7),"NNP","ronnie") -> (7,8,"NAME"),
    Token("Biggs",CharOffsets(8,8),"NNP","Biggs") -> (8,9,"SBJ"),
    Token("thumbed",CharOffsets(9,9),"VBD","thumbed") -> (9,0,"ROOT"),
    Token("his",CharOffsets(10,10),"PRP$","his") -> (10,11,"NMOD"),
    Token("nose",CharOffsets(11,11),"NN","nose") -> (11,9,"OBJ"),
    Token("at",CharOffsets(12,12),"IN","at") -> (12,11,"LOC"),
    Token("attempts",CharOffsets(13,13),"NNS","attempt") -> (13,12,"PMOD"),
    Token("to",CharOffsets(14,14),"TO","to") -> (14,13,"NMOD"),
    Token("bring",CharOffsets(15,15),"VB","bring") -> (15,14,"IM"),
    Token("him",CharOffsets(16,16),"PRP","him") -> (16,15,"OBJ"),
    Token("back",CharOffsets(17,17),"RB","back") -> (17,15,"DIR"),
    Token("to",CharOffsets(18,18),"TO","to") -> (18,15,"DIR"),
    Token("Britain",CharOffsets(19,19),"NNP","britain") -> (19,18,"PMOD"),
    Token("to",CharOffsets(20,20),"TO","to") -> (20,15,"PRP"),
    Token("serve",CharOffsets(21,21),"VB","serve") -> (21,20,"IM"),
    Token("his",CharOffsets(22,22),"PRP$","his") -> (22,23,"NMOD"),
    Token("sentence",CharOffsets(23,23),"NN","sentence") -> (23,21,"OBJ"),
    Token("for",CharOffsets(24,24),"IN","for") -> (24,23,"NMOD"),
    Token("the",CharOffsets(25,25),"DT","the") -> (25,28,"NMOD"),
    Token("Great",CharOffsets(26,26),"NNP","great") -> (26,27,"NAME"),
    Token("Train",CharOffsets(27,27),"NNP","train") -> (27,28,"NMOD"),
    Token("Robbery",CharOffsets(28,28),"NN","Robbery") -> (28,24,"PMOD"),
    Token(".",CharOffsets(29,29),".",".") -> (29,9,"P")
  ).toIndexedSeq
    
  val tokens = deps.map(_._1)
  val arcs = deps.map(_._2)
        
  val sentence = Sentence(tokens,
    SyntaxAnnotation(null, DependencyTree(tokens, arcs)),
      IEAnnotation(Vector(
        EntityMention("PER",6,8,null), 
        EntityMention("LOC",18,19,null), 
        EntityMention("ORG",25,28,null)),Vector(),Vector(),Vector()
      )
    )
  
  //println(sentence)

  (6 to 8).foreach { i =>
    (18 to 19).foreach { j =>
      println(DepUtils.findPath(i, j, sentence, true)._2)
      (25 to 28).foreach { k =>
        println(DepUtils.findPath(i, k, sentence, true)._2)
        println(DepUtils.findPath(j, k, sentence, true)._2)
      }
    }
  }
}

object EntityHackNormalization extends App {
  def loadEntityMap(filePath: String): Map[String, (String, String)] =
    Source.fromFile(filePath).getLines().map(_.split("\t")).map(a => (a.head, (a.tail.head, a.last))).toMap

  lazy val englishMap = loadEntityMap("./data/bbc/normalized_english.tsv")
  lazy val portugueseMap = loadEntityMap("./data/bbc/normalized_portuguese.tsv")

  def normalizeEnglish(name: String): Option[(String, String)] = englishMap.get(name)
  def normalizePortuguese(name: String): Option[(String, String)] = portugueseMap.get(name)


  englishMap.keys.take(100).foreach(k => println("[" + k + "]"))

  //println(portugueseMap.head)
}