package ml.wolfe.nlp.io
import ml.wolfe.nlp._

import scala.util.matching.Regex

/**
 * Created by narad on 7/31/14.
 */
class AnnReader(dir: String, pattern: String = ".*") extends Iterable[Document] {
    private val T_PATTERN1      = """(T[0-9]+)\t([^\t ]+) +([0-9]+) +([0-9]+)\t(.+)""".r
    private val T_PATTERN2      = """(T[0-9]+)\t([^\t ]+) +([0-9]+) +([0-9]+).*GSUBTYPE=\"([^ ]+)\" TYPE=\"([^ ]+)\".*""".r
    private val T_TIMEX_PATTERN = """(T[0-9]+)\ttimex2 +([0-9]+) +([0-9]+)\t([^\t]+)\tTVAL=\"([^ ]+)\".*""".r
    private val T_WORD_PATTERN  = """(T[0-9]+)\tEVENT_([^\t ]+) +([0-9]+) +([0-9]+)\t(.+)""".r
    private val R_PATTERN       = """(R[0-9]+)\t([^\t]+) +x:([^ ]+) +y:([^ ]+)""".r
    private val ROLE_PATTERN    = """ROLE_([^:]+):(T[0-9]+)""".r
    private val A_PATTERN       = """(A[0-9]+)\t([^\t]+) +(.+)""".r
    private val E_PATTERN       = """(E[0-9]+)\t([^:]+):([^\t ]+) (.*)""".r

  def iterator: Iterator[Document] = {
    val dreader = new DirectoryReader
    val fgroups = dreader.fileGroups(dir)
    fgroups.keys.filter(k => k.matches(pattern) && fgroups(k).size == 2).iterator.map { f =>
      mkAnnDocument(f + ".txt", f + ".ann")
    }
  }

  def mkAnnDocument(textFile: String, annFile: String): Document = {
    println("Reading pair <%s> and <%s>".format(textFile, annFile))
    val lines = scala.io.Source.fromFile(textFile).getLines().toArray
    val annotations = scala.io.Source.fromFile(annFile).getLines().toArray
    val text = lines.mkString("\n")

    // Initial collection of entity mentions using the data's character offsets
    val entities = annotations.collect {
      case T_PATTERN1(id, name, start, end) => {
        EntityMention(name, start.toInt, end.toInt, id = id)
      }
      case T_PATTERN2(id, name, start, end, etype, esubtype) => {
        EntityMention(name, start.toInt, end.toInt, id = id)
      }
      case T_TIMEX_PATTERN(id, start, end, ttext, value) => {
        EntityMention("TIMEX", start.toInt, end.toInt, id = id)
      }
      case T_WORD_PATTERN(id, name, start, end, ttext) => {
        EntityMention(name, start.toInt, end.toInt, id = id)
      }
    }.toIndexedSeq

    val d = SISTAProcessors.mkDocument(text)
    d copy(sentences = d.sentences.map { s =>
      addEvents(addRelations(addEntities(s, entities), annotations), annotations)
    })
  }

  def addEntities(s: Sentence, e: IndexedSeq[EntityMention]): Sentence = {
    val entities = e.collect {
      case t if containsEntity(s, t) => {
        val si = s.tokens.indexWhere(_.offsets.start == t.start)
        val ei = s.tokens.indexWhere(_.offsets.end == t.end)
        t.copy(start = si, end = ei)
      }
    }
    s.copy(ie = IEAnnotation(entities, relationMentions = null, eventMentions = null, semanticFrames = null))
  }

  def addRelations(s: Sentence, annotations: Array[String]): Sentence = {
    val relations = annotations.collect { CachedPartialFunction[String, RelationMention](containsRelation(s,_)) }.toIndexedSeq
    s.copy(ie = s.ie.copy(relationMentions = relations))
  }

  def addEvents(s: Sentence, annotations: Array[String]): Sentence = {
    val events = annotations.collect { CachedPartialFunction[String, EventMention](containsEvent(s,_)) }.toIndexedSeq
    s.copy(ie = s.ie.copy(eventMentions = events))
  }

  def containsEntity(s: Sentence, t: EntityMention): Boolean = {
    val si = s.tokens.indexWhere(_.offsets.start == t.start)
    val ei = s.tokens.indexWhere(_.offsets.end == t.end)
    si > -1 && ei > -1
  }

  def containsRole(entities: IndexedSeq[EntityMention], a1: String, a2: String): Boolean = {
    val ai1 = entities.indexWhere(_.id == a1)
    val ai2 = entities.indexWhere(_.id == a2)
    ai1 > -1 && ai2 > -1
  }

  def containsRelation(s: Sentence, ann: String): Option[RelationMention] = {
    ann match {
      case R_PATTERN(id, name, a1, a2) => {
        val a1idx = s.ie.entityMentions.indexWhere(_.id == a1)
        val a2idx = s.ie.entityMentions.indexWhere(_.id == a2)
        Some(RelationMention(name, a1idx, a2idx, id = id))
      }
      case _=> None
    }
  }

  def containsEvent(s: Sentence, ann: String): Option[EventMention] = {
    ann match {
      case E_PATTERN(id, name, trigger, arguments) => {
        val tidx = s.ie.entityMentions.indexWhere(_.id == trigger)
        val roles = arguments.split(" ").collect {
          case ROLE_PATTERN(role, term) if containsRole(s.ie.entityMentions, trigger, term) => {
            val aidx = s.ie.entityMentions.indexWhere(_.id == term)
            RoleMention(role, s.ie.entityMentions(aidx))
          }
        }
        if (!roles.isEmpty) {
          Some(EventMention(name, s.ie.entityMentions(tidx), roles))
        }
        else {
          None
        }
      }
      case _=> None
    }
  }
}

object AnnReader {

  def main(args: Array[String]) {
    val reader = new AnnReader(args(0))
    for (d <- reader) {
      println("Document: ")
      for (s <- d.sentences) {
        var tcount = 0
        for (t <- s.tokens) {
          println(tcount + ": " + t)
          tcount += 1
        }
        println("Entities:\n" + s.ie.entityMentions.mkString("\n") + "\n")
        println("Relations:\n" + s.ie.entityMentions.mkString("\n") + "\n")
        println("Events:\n" + s.ie.eventMentions.mkString("\n") + "\n")
        println("Dependencies:\n" + s.syntax.dependencies)
        println("Parse:\n" + s.syntax.tree)
        println
      }
    }
  }
}

case class CachedPartialFunction[A, B](f: A => Option[B]) extends PartialFunction[A, B] {
  private var cacheArg   : A         = _
  private var cacheResult: Option[B] = None

  def cache(x: A) = {
    if (x != cacheArg) {
      cacheArg = x
      cacheResult = f(cacheArg)
    }
  }

  def isDefinedAt(x: A) = {
    cache(x)
    cacheResult.isDefined
  }

  def apply(x: A) = {
    cache(x)
    cacheResult.get
  }
}
