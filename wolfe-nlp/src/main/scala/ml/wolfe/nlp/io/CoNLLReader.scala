package ml.wolfe.nlp.io

import ml.wolfe.nlp._
import ml.wolfe.nlp.semantics.{Predicate, SemanticFrame, SemanticRole}
import ml.wolfe.nlp.syntax.{Arc, ModifiedCollinsHeadFinder, DependencyTree, ConstituentTree}
import ml.wolfe.nlp.ie.{CorefMention, CorefAnnotation, EntityMention}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.matching.Regex

/**
 * Created by narad on 8/12/14.
 */
class CoNLLReader(filename: String, delim: String = "\t") extends Iterable[Sentence] {

  def isNumber(str: String): Boolean = {
    str.matches("[0-9]+")
  }

  def iterator: Iterator[Sentence] = {
    val reader = new ChunkReader(filename)
    reader.iterator.map { s =>
      val lines = s.split("\n")
      val cols = lines.head.split(delim)
      val numFields = cols.size
      //      if (isNumber(cols(1)) && isNumber(cols(2))) {
      //        // Identified as a file from the CoNLL 2011 Coreference Task
      //        fromCoNLL2011(lines)
      //      }
      if (numFields >= 12)
      // Identified as a file from the CoNLL SRL tasks of 2008/2009
        fromCoNLL2009(lines)
      else if (numFields == 10)
      // Identified as a file from the CoNLL-X Dependency Parsing shared task (2006/2007)
        fromCoNLLX(lines)
      else if (numFields == 4)
      // Identified as a file from the CoNLL 2003 NER shared task
        fromCoNLL2003(lines)
      else
      // Identified as a file from the CoNLL 2000 part-of-speech tagging shared task
        fromCoNLL2000(lines)
    }
  }


  def fromCoNLL2009(lines: IndexedSeq[String]): Sentence = {
    // ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
    val cells = lines.map(_.split("\t"))
    val cols = cells.head.size
    val tokens = cells.map { c =>
      Token(c(1), CharOffsets(c(0).toInt, c(0).toInt), posTag = c(4), lemma = c(2))
    }
    val preds = cells.zipWithIndex.filter(_._1(12) == "Y").map { case (l, i) => Predicate(i+1, tokens(i), l(13)) }
    val argsets = (14 to 13 + preds.size).map { i =>
      cells.zipWithIndex.flatMap { case (row, ri) => row(i) match {
        case "_" => None
        case x => Some(SemanticRole(start = ri+1, role = x))
      }
      }
    }
    val frames = preds.zip(argsets).map { case (p, a) => SemanticFrame(p, a) }
    assert(preds.size == argsets.size)
    val dependencies = DependencyTree(tokens, cells.zipWithIndex.map { case (c, i) => Arc(i + 1, c(8).toInt, Some(c(10))) })
    Sentence(tokens, syntax = SyntaxAnnotation(tree = null, dependencies = dependencies), ie = IEAnnotation.empty.copy(semanticFrames = frames))
  }

  def fromCoNLLX(lines: IndexedSeq[String]): Sentence = {
    val cells = lines.map(_.split("\t"))
    val tokens = cells.map { c =>
      Token(c(1), CharOffsets(c(0).toInt, c(0).toInt), posTag = c(4), lemma = c(2))
    }
    val dependencies = DependencyTree(tokens, cells.zipWithIndex.map { case (c, i) => Arc(i + 1, c(6).toInt, Some(c(7))) })
    Sentence(tokens, syntax = SyntaxAnnotation(tree = null, dependencies = dependencies))
  }

  def fromCoNLL2003(lines: IndexedSeq[String]): Sentence = {
    val cells = lines.map(_.split(" "))
    def join(sofar: List[Token], c: Array[String]): List[Token] = sofar match {
      case Nil => Token(c(0), CharOffsets(0, c(0).length), posTag = c(1)) :: Nil
      case h :: t => Token(c(0), CharOffsets(h.offsets.end + 1, h.offsets.end + 1 + c(0).length), posTag = c(1)) :: h :: t
    }

    val tokens = cells.foldLeft(List.empty[Token])(join).reverse
    val ner = cells.map(_.apply(3))
    val mentions = CoNLLReader.collectMentions(ner)
    Sentence(tokens.toIndexedSeq, ie = IEAnnotation(entityMentions = mentions.toIndexedSeq))
  }

  def fromCoNLL2000(lines: IndexedSeq[String]): Sentence = {
    ???
  }
}

object CoNLLReader {

  def asDocs(fileName: String, delim: String = " ") = new CoNLLReader(fileName, delim).map({
    case sentence => Document(sentence.toText, IndexedSeq(sentence))
  })

  def collectMentions(t: Seq[String]): Seq[EntityMention] = {
    t.zipWithIndex.foldLeft(ListBuffer[EntityMention]())((mentions: ListBuffer[EntityMention], tokenWithIndex) => {
      val (label, ix) = tokenWithIndex
      val Array(prefix, labelType) = if (label == "O") Array("O", "O") else label.split("-")

      prefix match {
        case "O" => mentions
        case "B" => mentions :+ EntityMention(labelType, ix, ix + 1)
        case "I" =>
          if (mentions.isEmpty) mentions :+ EntityMention(labelType, ix, ix + 1)
          else {
            val last = mentions.last
            if (last.end == ix && last.label == labelType)
              mentions.updated(mentions.length - 1, mentions.last.expandRight(1))
            else mentions :+ EntityMention(labelType, ix, ix + 1)
          }
      }
    }).toSeq
  }

  def main(args: Array[String]) {
    val data = new CoNLLReader("/Users/sriedel/corpora/conll03/eng.train", " ").take(100).toIndexedSeq
    val doc = Document(data(1).toText, IndexedSeq(data(1)))
    println(data(1))
  }
}

class CoNLL2011Reader(filename: String, delim: String = "\t") extends Iterable[Document] {

  def iterator: Iterator[Document] = {
    new ChunkReader(filename, delim = "^#end\\ document.*").map(mkCoNLL2011Document(_)).iterator
  }

  def mkCoNLL2011Document(chunk: String): Document = {
    val chunks = chunk.split("\n\n")
    val sents = chunks.map { chunk =>
      val grid = chunk.split("\n").filter(!_.startsWith("#")).map(_.replaceAll(" +", "\t").split("\t"))
      val numCols = grid(0).size
      val speaker = grid(0)(9)
      val words = (0 until grid.size).map(grid(_)(3))
      val pos = (0 until grid.size).map(grid(_)(4))
      val lemma = (0 until grid.size).map(grid(_)(6))
      val tokens = words.zip(pos).zipWithIndex.map { case (p, i) => Token(p._1, CharOffsets(i, i + 1), p._2, lemma = lemma(i))}
      val csyntax = (0 until grid.size).map(grid(_)(5))
      val ner = (0 until grid.size).map(i => fixNestedField(grid(i)(10)))

      val csyntaxCleaned = csyntax.mkString(" ").replaceAll("\\*", " * ").replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").replaceAll(" +", " ")
      var tc = -1
      val csyntaxStr = csyntaxCleaned.map(c => if (c == '*') {tc += 1; "(" + pos(tc) + " " + words(tc) + ")"} else c.toString).mkString("")
//      println("Cleaned syntax string: " + csyntaxStr)
      val ctree = ModifiedCollinsHeadFinder.annotate(ConstituentTreeFactory.stringToTree(csyntaxStr).get)
      assert(ctree != null, "Null constituent tree")

      val entities = readStackFormatEntities(ner.toList)


      // 6,7,8 predicate info
      //11 - len-2 is arguments
      val predicates = (0 until words.size).collect {
        case i if grid(i)(6) != "-" => Predicate(i, tokens(i), grid(i)(6) + "." + grid(i)(7))
      }
      val args = (11 until numCols-1).map {i =>
        readStackFormatSRL((0 until words.size).map(j => fixNestedField(grid(j)(i))).toList)
      }
      val frames = predicates.zip(args).map { case(p,r) => SemanticFrame(predicate = p, roles = r)}


      val sense = (0 until grid.size).map(grid(_)(7))
      //ctree.toDependencyTree
      Sentence(tokens, syntax = SyntaxAnnotation(tree = ctree, dependencies = ctree.toDependencyTree),
                       ie = IEAnnotation(entityMentions = entities, semanticFrames = frames),
                       speaker = Some(speaker))
    }.toIndexedSeq


    val mentions = chunks.zipWithIndex.map { case(chunk, cidx) =>
      val grid = chunk.split("\n").filter(!_.startsWith("#")).map(_.replaceAll(" +", "\t").split("\t"))
      readStackFormatCoref(grid.map(_.last).toList, sidx = cidx)
    }.flatten

    val chunkFilename = chunk.split("\n").find(_.startsWith("#begin document")) //Option[String] = None
    Document(source = chunks.mkString("\n"), filename = chunkFilename, sentences = sents, coref = CorefAnnotation(mentions = mentions.toSeq))
  }

  //.map(s => if (!s.contains("*")) s.replaceFirst("\\)", "*)") else s)
  def fixNestedField(str: String): String = {
    if (!str.contains("*")) str.replaceFirst("\\)", "*)")
    else if (str.startsWith("(")) str.replaceAll("\\(", "*(").substring(1)
    else str // str.replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").replaceAll(" +", " ")
  }


  def readStackFormatEntities(l: List[String]): Array[EntityMention] = {
    stackReader(l.toArray, startPattern="""\(([^\*]+)\*""".r, endPattern = """(\*)\)""".r, matchString = false).map{
      case(start, end, label) => EntityMention(start = start, end = end+1, label = label)
    }
  }

  def readStackFormatSRL(l: List[String]): Array[SemanticRole] = {
//    println(l.mkString(", "))
    stackReader(l.toArray, startPattern="""\(([^\*]+)\*""".r, endPattern = """(\*)\)""".r, matchString = false).map{
      case(pidx, aidx, role) => SemanticRole(start = pidx, end = aidx+1, role = role)
    }
  }

  def readStackFormatCoref(l: List[String], sidx: Int): Array[CorefMention] = {
//    println(l.mkString(", "))
    stackReader(l.toArray, startPattern="""\(([0-9]+)""".r, endPattern = """([0-9]+)\)""".r, matchString = true).map{
      case(start, end, cluster) => CorefMention(cluster.toInt, sentence = sidx, start = start, end = end+1, head = -1)
    }
  }

  def stackReader(seq: Array[String], startPattern: Regex, endPattern: Regex, matchString: Boolean): Array[(Int, Int, String)] = {
    var starts = (0 until seq.size).map(i => (startPattern findAllIn seq(i)).matchData.toArray.map(m => (i, m.group(1)))).flatten.toArray
    var ends = (0 until seq.size).map(i => (endPattern findAllIn seq(i)).matchData.toArray.map(m => (i, m.group(1)))).flatten.toArray
    val founds = new ArrayBuffer[(Int, Int, String)]
    founds ++= findMatches(starts, ends, matchString = matchString)
    founds.toArray
  }

  def findMatches(starts: Array[(Int, String)], ends: Array[(Int, String)], matchString: Boolean): Array[(Int, Int, String)] = {
    val found = new ArrayBuffer[(Int, Int, String)]
    val buffer = new ArrayBuffer[(Int, String)]
    buffer ++= starts.reverse
    ends.foreach { e =>
      val endIdx = if (matchString) buffer.indexWhere(s => s._2 == e._2 && s._1 <= e._1) else buffer.indexWhere(s => s._1 <= e._1)
      val s = buffer(endIdx)
      buffer.remove(endIdx)
      found += ((s._1, e._1, s._2))
    }
    found.toArray
  }

  def recursiveStackReader(list: List[String], startPattern: Regex, endPattern: Regex): List[(Int, Int, String)] = {
    if (list.head.matches(" ")) {

    }
    ???
  }
}


object CoNLL2011Reader extends App {
  val mlens = new mutable.HashMap[Int, Int].withDefaultValue(0)
  val dlens = new mutable.HashMap[Int, Int].withDefaultValue(0)
  val slensByMentions = new mutable.HashMap[Int, Int].withDefaultValue(0)
  val corefDistances = new mutable.HashMap[Int, Int].withDefaultValue(0)
  val mentionConsts = new mutable.HashMap[String, Int].withDefaultValue(0)
  var numMentions = 0
  var numNested = 0
  var numCrossing = 0
  var numASpans = 0
  var numNSpans = 0
  for (doc <- new CoNLL2011Reader(args(0))) {
    dlens(doc.sentences.size) += 1
    for (m <- doc.coref.mentions) {
      mlens(m.width) += 1
      numMentions += 1
      if (doc.sentences(m.sentence).syntax.tree.containsSpan(m.start, m.end)) numASpans += 1
      if (doc.sentences(m.sentence).syntax.tree.spansAt(m.start, m.end).count(_.label.startsWith("N")) > 0) numNSpans += 1
      doc.sentences(m.sentence).syntax.tree.spansAt(m.start, m.end).foreach { sp => mentionConsts(sp.label) += 1}
    }
    for (m1 <- doc.coref.mentions; m2 <- doc.coref.mentions if m1 != m2) {
      if (m1.nests(m2)) numNested += 1
      if (m1.crosses(m2)) numCrossing += 1
      if (doc.coref.shareCluster(m1, m2)) {
        corefDistances(doc.coref.distanceInMentions(m1, m2)) += 1
      }
    }
    for (sidx <- 0 until doc.sentences.size) slensByMentions(doc.coref.mentions.count(_.sentence == sidx)) += 1
  }

  println("Document Lengths (# sentences):")
  dlens.keys.toArray.sortBy(_ * 1.0).foreach(k => println(k + ":\t" + dlens(k)))
  println("\n\n")

  println("Mention Lengths:")
  mlens.keys.toArray.sortBy(_ * 1.0).foreach(k => println(k + ":\t" + mlens(k)))
  println("\n\n")

  println("Mentions per Sentence:")
  slensByMentions.keys.toArray.sortBy(_ * 1.0).foreach(k => println(k + ":\t" + slensByMentions(k)))
  println("\n\n")

  println("Distance of Coref Links (by # interceding mentions):")
  corefDistances.keys.toArray.sortBy(_ * 1.0).foreach(k => println(k + ":\t" + corefDistances(k)))
  println("\n\n")

  println("Constituents which appear as mentions:")
  mentionConsts.keys.toArray.sortBy(mentionConsts(_)).foreach(k => println(k + ":\t" + mentionConsts(k)))
  println

  println("Number of nested mentions = %d / %d = %.1f%%".format(numNested, numMentions, numNested * 100.0 / numMentions))
  println("Number of crossing mentions = %d / %d = %.1f%%".format(numCrossing, numMentions, numCrossing * 100.0 / numMentions))
  println("Number of mentions corresponding to constituent spans = %d / %d = %.1f%%".format(numASpans, numMentions, numASpans * 100.0 / numMentions))
  println("Number of mentions corresponding to NP spans = %d / %d = %.1f%%".format(numNSpans, numMentions, numNSpans * 100.0 / numMentions))
  println("Done.")

}


object CoNLL2011Test extends App {

  val sample = """wb/eng/00/eng_0010   4    0         But     CC                   (TOP(S*        -    -   -    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4    1           I    PRP                      (NP*)       -    -   -    _jules_  *    (ARG0*)        *             *             *             *     -
                 |wb/eng/00/eng_0010   4    2         bet    VBP                      (VP*       bet  01   2    _jules_  *       (V*)        *             *             *             *     -
                 |wb/eng/00/eng_0010   4    3        such     JJ               (SBAR(S(NP*        -    -   -    _jules_  *    (ARG2*     (ARG1(R-ARG2*)             *             *             *     (4|(5
                 |wb/eng/00/eng_0010   4    4    concerns    NNS                         *)       -    -   1    _jules_  *         *         *)            *             *             *     -
                 |wb/eng/00/eng_0010   4    5         are    VBP                      (VP*        be  01   1    _jules_  *         *       (V*)            *             *             *     -
                 |wb/eng/00/eng_0010   4    6         far     JJ                    (ADJP*        -    -   -    _jules_  *         *    (ARG2*)            *             *             *     5)
                 |wb/eng/00/eng_0010   4    7        from     IN                      (PP*        -    -   -    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4    8         the     DT                   (NP(NP*        -    -   -    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4    9       minds    NNS                         *)       -    -   3    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4   10          of     IN                      (PP*        -    -   -    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4   11    military     JJ                   (NP(NP*        -    -   -    _jules_  *         *         *        (ARG1*        (ARG0*        (ARG0*    (3
                 |wb/eng/00/eng_0010   4   12     leaders    NNS                         *)       -    -   1    _jules_  *         *         *             *)            *)            *)    -
                 |wb/eng/00/eng_0010   4   13         who     WP               (SBAR(WHNP*)       -    -   -    _jules_  *         *         *      (R-ARG1*)     (R-ARG0*)     (R-ARG0*)    4)
                 |wb/eng/00/eng_0010   4   14         can     MD                    (S(VP*        -    -   -    _jules_  *         *         *    (ARGM-MOD*)            *             *     -
                 |wb/eng/00/eng_0010   4   15         not     RB                         *        -    -   -    _jules_  *         *         *    (ARGM-NEG*)            *             *     -
                 |wb/eng/00/eng_0010   4   16          be     VB                      (VP*        be  01   1    _jules_  *         *         *           (V*)            *             *     -
                 |wb/eng/00/eng_0010   4   17    bothered     JJ                    (ADJP*        -    -   -    _jules_  *         *         *        (ARG2*             *             *     -
                 |wb/eng/00/eng_0010   4   18          to     TO                    (S(VP*        -    -   -    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4   19       equip     VB                      (VP*     equip  01   1    _jules_  *         *         *             *           (V*)            *     -
                 |wb/eng/00/eng_0010   4   20          or     CC                         *        -    -   -    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4   21      supply     VB                         *    supply  01   -    _jules_  *         *         *             *             *           (V*)    -
                 |wb/eng/00/eng_0010   4   22       their   PRP$                      (NP*        -    -   -    _jules_  (THING*         *         *             *        (ARG1*        (ARG1*    (3)
                 |wb/eng/00/eng_0010   4   23         own     JJ                         *        -    -   -    _jules_  *         *         *             *             *             *     -
                 |wb/eng/00/eng_0010   4   24      troops    NNS                         *)       -    -   -    _jules_  *)         *         *             *             *)            *)    -
                 |wb/eng/00/eng_0010   4   25    properly     RB   (ADVP*))))))))))))))))))       -    -   -    _jules_  *         *)        *             *)   (ARGM-MNR*)   (ARGM-MNR*)    3)
                 |wb/eng/00/eng_0010   4   26           .      .                        *))       -    -   -    _jules_  *         *         *             *             *             *     -
                 |""".stripMargin
  val reader = new CoNLL2011Reader(filename = null)
  val doc = reader.mkCoNLL2011Document(sample)
  println(doc)
  println("Coref:")
  println(doc.coref.mentions.mkString("\n"))
  println("SRL:")
  doc.sentences.foreach(s => s.ie.semanticFrames.foreach(f => println(f)))
  println("NER:")
  doc.sentences.foreach(s => s.ie.entityMentions.foreach(e => println(e)))


}











/*
      val nerCleaned = ner.mkString(" ").replaceAll("\\*", " * ").replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").replaceAll(" +", " ")

      var nc = -1
      val nerStr = "(XX " + nerCleaned.map(c => if (c == '*') {nc += 1; "(XX " + words(nc) + ")"} else c.toString).mkString("") + ")"
      val nertree = ConstituentTree.stringToTree(nerStr)
      assert(nertree != null, "Null NER tree")
      val mentions = nertree.toSpans.view.filter(_.label != "XX").map(s => EntityMention(s.label, s.start, s.end))

 */


//    val mentions = new ArrayBuffer[CorefMention]
//    val corefs = chunks.zipWithIndex.map { case(chunk, sidx) =>
//      val grid = chunk.split("\n").filter(!_.startsWith("#")).map(_.replaceAll(" +", "\t").split("\t"))
//      val slen = grid.size
//      val CSTART_PATTERN = """\(([0-9]+)""".r
//      val CEND_PATTERN = """([0-9]+)\)""".r
//      val buffer = new ArrayBuffer[(Int, Int)]
//      (0 until slen).foreach { i =>
//        val ccell = grid(i).last
//        buffer ++= (CSTART_PATTERN findAllIn ccell).matchData.toArray.map(m => (i, m.group(1).toInt))
//        (CEND_PATTERN findAllIn ccell).matchData.toArray.map(m => (i, m.group(1).toInt)).foreach { e =>
//          val stidx = buffer.indexWhere(_._2 == e._2)
//          val s = buffer(stidx)
//          buffer.remove(stidx)
//          mentions += CorefMention(s._2, sidx, s._1, e._1+1)
//        }
//      }
//    }.toIndexedSeq


//  def stackReader(list: List[String], startPattern: Regex, endPattern: Regex): List[(Int, Int, String)] = {
//    val found = new ArrayBuffer[(Int, Int, String)]
//    val buffer = new ArrayBuffer[(Int, String)]
//    (0 until list.size).foreach { i =>
//      buffer ++= (startPattern findAllIn list(i)).matchData.toArray.map(m => (i, m.group(1)))
//      val ends = (endPattern findAllIn list(i)).matchData.toArray.map(m => (i, m.group(1)))
//      ends.foreach { e =>
//        val endIdx = buffer.indexWhere(_._2 == e._2)
//        val s = buffer(endIdx)
//        buffer.remove(endIdx)
//        found += ((s._1, e._1, s._2))
//      }
//    }
//    found.toList
//  }


//      new ChunkReader(filename).toArray.map(_.split("\n").filter(!_.startsWith("#")).mkString("\n")).groupBy { c =>
//      val l = c.split("\n")//.filter(!_.startsWith("#"))
//      if (l.isEmpty || l.size == 1) {
//        null
//      }
//      else {
//        val first = l.head
//        first.substring(0, first.indexOf(" "))
//      }
//    }
//    files.keys.filter(_ != null).map{f => mkCoNLL2011Document(files(f))}.iterator
//  }

//  def mkCoNLL2011Document(chunks: Array[String]): Document = {

/*      (5  59
      (8 44
       9 44)
      10 59)
      */

//      println("STARTS: " + starts.mkString(", "))
//      println("ENDS: " + ends.mkString(", "))
//      var mentions = new ArrayBuffer[CorefMention]
//      for (w <- 0 until slen; i <- 0 until slen if (w+i) < slen) {
//
//      }
//      val mentions = starts.map { s =>
//        val e = ends.find(e => s._2 == e._2).get
//        ends = ends.filter(_ != e)
//        CorefMention(s._2, sidx, s._2, e._2)
//      }.toSeq


/*
    t.coref.mentions.zipWithIndex.foreach { case(c, cidx) =>
      val dist = t.coref.mentions.slice(0, cidx).reverse.find(_.clusterID == c.clusterID) match {
        case Some(m) => {
          println
          cidx - t.coref.mentions.indexOf(m)
        }
        case _ => 0
      }
      println("D: " + dist)
    }

          println("Mention: " + c + "\n  -- links to --  \n" + m)
          println("REF: " + t.coref.mentionTokens(c, t).mkString(" "))
          println("ANT: " + t.coref.mentionTokens(m, t).mkString(" "))
          println("DOC: " + t.filename)
*/
//          println("REF Sentence: " + t.sentences(c.sentence).tokens.mkString(" "))
//          println("ANT Sentence: " + t.sentences(m.sentence).tokens.mkString(" "))
//          println("Sentence size: " + t.sentences(c.sentence).size)
//          println("Mentions in Sentence: " + t.coref.mentions.filter(_.sentence == c.sentence).mkString("\n"))


//      cells.zipWithIndex.filter(_._1(6) == "Y").map { case (l, i) => Predicate(i+1, tokens(i), l(13)) }
//      val argsets = (14 to 13 + preds.size).map { i =>
//        cells.zipWithIndex.flatMap { case (row, ri) => row(i) match {
//          case "_" => None
//          case x => Some(SemanticRole(ri+1, x))
//        }
//        }
//      }
//      val frames = preds.zip(argsets).map { case (p, a) => SemanticFrame(p, a) }
