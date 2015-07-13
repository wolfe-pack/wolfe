package ml.wolfe.nlp.io

import java.io.FileInputStream

import ml.wolfe.SimpleFeatureIndex
import ml.wolfe.nlp.io.DocumentProtos.Riedel2010Relation
import ml.wolfe.nlp.{TokenSplitter, Sentence}
import ml.wolfe.nlp.ie.RelationMention
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term.domain


object Riedel2010Reader {
  def read(path: String) = {

    // Load data
    val fis = new FileInputStream(path)
    val rels = Stream.continually {
      DocumentProtos.Riedel2010Relation.parseDelimitedFrom(fis)
    }.takeWhile(_.isDefined).map(_.get).toArray

    // Set up domains
    /*case class EntityPair(arg1:String, arg2:String)
    val _entityPairs = rels.map{ rel:Riedel2010Relation => EntityPair(rel.sourceGuid, rel.destGuid)}
    implicit val EntityPairs = _entityPairs.toDom*/

    val _labels = rels.map{ rel:Riedel2010Relation => rel.relType}.toSeq.distinct
    val _Labels = _labels.toDom withOOV "[OOV]"

    new {
      implicit val Labels = _Labels
      val labels = _labels.map(Labels.Const)

      case class Bag(/*entityPair: EntityPairs.Term, */label:Labels.Term, instances:Seq[Seq[String]])
      val bags = rels.map{ rel:Riedel2010Relation => Bag(
        //entityPair = EntityPairs.Const(EntityPair(rel.sourceGuid, rel.destGuid)),
        label = Labels.Const(rel.relType),
        instances = rel.mention.map(_.feature)
      )}
    }
  }


}

private object DocumentProtos{

  final case class Riedel2010Document (
                                        `filename`: String = "",
                                        `sentences`: scala.collection.immutable.Seq[Riedel2010Document.Sentence] = Vector.empty[Riedel2010Document.Sentence]
                                        ) extends com.google.protobuf.GeneratedMessageLite
  with com.google.protobuf.MessageLite.Builder
  with net.sandrogrzicic.scalabuff.Message[Riedel2010Document]
  with net.sandrogrzicic.scalabuff.Parser[Riedel2010Document] {

    def setSentences(_i: Int, _v: Riedel2010Document.Sentence) = copy(`sentences` = `sentences`.updated(_i, _v))
    def addSentences(_f: Riedel2010Document.Sentence) = copy(`sentences` = `sentences` :+ _f)
    def addAllSentences(_f: Riedel2010Document.Sentence*) = copy(`sentences` = `sentences` ++ _f)
    def addAllSentences(_f: TraversableOnce[Riedel2010Document.Sentence]) = copy(`sentences` = `sentences` ++ _f)

    def clearSentences = copy(`sentences` = Vector.empty[Riedel2010Document.Sentence])

    def writeTo(output: com.google.protobuf.CodedOutputStream) {
      output.writeString(1, `filename`)
      for (_v <- `sentences`) output.writeMessage(2, _v)
    }

    def getSerializedSize = {
      import com.google.protobuf.CodedOutputStream._
      var __size = 0
      __size += computeStringSize(1, `filename`)
      for (_v <- `sentences`) __size += computeMessageSize(2, _v)

      __size
    }

    def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Riedel2010Document = {
      import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
      var __filename: String = ""
      val __sentences: scala.collection.mutable.Buffer[Riedel2010Document.Sentence] = `sentences`.toBuffer

      def __newMerged = Riedel2010Document(
        __filename,
        Vector(__sentences: _*)
      )
      while (true) in.readTag match {
        case 0 => return __newMerged
        case 10 => __filename = in.readString()
        case 18 => __sentences += readMessage[Riedel2010Document.Sentence](in, Riedel2010Document.Sentence.defaultInstance, _emptyRegistry)
        case default => if (!in.skipField(default)) return __newMerged
      }
      null
    }

    def mergeFrom(m: Riedel2010Document) = {
      Riedel2010Document(
        m.`filename`,
        `sentences` ++ m.`sentences`
      )
    }

    def getDefaultInstanceForType = Riedel2010Document.defaultInstance
    def clear = getDefaultInstanceForType
    def isInitialized = true
    def build = this
    def buildPartial = this
    def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
    override def getParserForType = this
    def newBuilderForType = getDefaultInstanceForType
    def toBuilder = this
    def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
  }

  object Riedel2010Document {
    @scala.beans.BeanProperty val defaultInstance = new Riedel2010Document()

    def parseFrom(data: Array[Byte]): Riedel2010Document = defaultInstance.mergeFrom(data)
    def parseFrom(data: Array[Byte], offset: Int, length: Int): Riedel2010Document = defaultInstance.mergeFrom(data, offset, length)
    def parseFrom(byteString: com.google.protobuf.ByteString): Riedel2010Document = defaultInstance.mergeFrom(byteString)
    def parseFrom(stream: java.io.InputStream): Riedel2010Document = defaultInstance.mergeFrom(stream)
    def parseDelimitedFrom(stream: java.io.InputStream): Option[Riedel2010Document] = defaultInstance.mergeDelimitedFromStream(stream)

    val FILENAME_FIELD_NUMBER = 1
    val SENTENCES_FIELD_NUMBER = 2

    def newBuilder = defaultInstance.newBuilderForType
    def newBuilder(prototype: Riedel2010Document) = defaultInstance.mergeFrom(prototype)

    final case class Sentence (
                                `tokens`: scala.collection.immutable.Seq[Token] = Vector.empty[Token],
                                `mentions`: scala.collection.immutable.Seq[Mention] = Vector.empty[Mention],
                                `depTree`: Option[DepTree] = None
                                ) extends com.google.protobuf.GeneratedMessageLite
    with com.google.protobuf.MessageLite.Builder
    with net.sandrogrzicic.scalabuff.Message[Sentence]
    with net.sandrogrzicic.scalabuff.Parser[Sentence] {

      def setTokens(_i: Int, _v: Token) = copy(`tokens` = `tokens`.updated(_i, _v))
      def addTokens(_f: Token) = copy(`tokens` = `tokens` :+ _f)
      def addAllTokens(_f: Token*) = copy(`tokens` = `tokens` ++ _f)
      def addAllTokens(_f: TraversableOnce[Token]) = copy(`tokens` = `tokens` ++ _f)
      def setMentions(_i: Int, _v: Mention) = copy(`mentions` = `mentions`.updated(_i, _v))
      def addMentions(_f: Mention) = copy(`mentions` = `mentions` :+ _f)
      def addAllMentions(_f: Mention*) = copy(`mentions` = `mentions` ++ _f)
      def addAllMentions(_f: TraversableOnce[Mention]) = copy(`mentions` = `mentions` ++ _f)
      def setDepTree(_f: DepTree) = copy(`depTree` = Some(_f))

      def clearTokens = copy(`tokens` = Vector.empty[Token])
      def clearMentions = copy(`mentions` = Vector.empty[Mention])
      def clearDepTree = copy(`depTree` = None)

      def writeTo(output: com.google.protobuf.CodedOutputStream) {
        for (_v <- `tokens`) output.writeMessage(1, _v)
        for (_v <- `mentions`) output.writeMessage(2, _v)
        if (`depTree`.isDefined) output.writeMessage(3, `depTree`.get)
      }

      def getSerializedSize = {
        import com.google.protobuf.CodedOutputStream._
        var __size = 0
        for (_v <- `tokens`) __size += computeMessageSize(1, _v)
        for (_v <- `mentions`) __size += computeMessageSize(2, _v)
        if (`depTree`.isDefined) __size += computeMessageSize(3, `depTree`.get)

        __size
      }

      def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Sentence = {
        import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
        val __tokens: scala.collection.mutable.Buffer[Token] = `tokens`.toBuffer
        val __mentions: scala.collection.mutable.Buffer[Mention] = `mentions`.toBuffer
        var __depTree: Option[DepTree] = `depTree`

        def __newMerged = Sentence(
          Vector(__tokens: _*),
          Vector(__mentions: _*),
          __depTree
        )
        while (true) in.readTag match {
          case 0 => return __newMerged
          case 10 => __tokens += readMessage[Token](in, Token.defaultInstance, _emptyRegistry)
          case 18 => __mentions += readMessage[Mention](in, Mention.defaultInstance, _emptyRegistry)
          case 26 => __depTree = Some(readMessage[DepTree](in, __depTree.orElse({
            __depTree = DepTree.defaultInstance
            __depTree
          }).get, _emptyRegistry))
          case default => if (!in.skipField(default)) return __newMerged
        }
        null
      }

      def mergeFrom(m: Sentence) = {
        Sentence(
          `tokens` ++ m.`tokens`,
          `mentions` ++ m.`mentions`,
          m.`depTree`.orElse(`depTree`)
        )
      }

      def getDefaultInstanceForType = Sentence.defaultInstance
      def clear = getDefaultInstanceForType
      def isInitialized = true
      def build = this
      def buildPartial = this
      def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
      override def getParserForType = this
      def newBuilderForType = getDefaultInstanceForType
      def toBuilder = this
      def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
    }

    object Sentence {
      @scala.beans.BeanProperty val defaultInstance = new Sentence()

      def parseFrom(data: Array[Byte]): Sentence = defaultInstance.mergeFrom(data)
      def parseFrom(data: Array[Byte], offset: Int, length: Int): Sentence = defaultInstance.mergeFrom(data, offset, length)
      def parseFrom(byteString: com.google.protobuf.ByteString): Sentence = defaultInstance.mergeFrom(byteString)
      def parseFrom(stream: java.io.InputStream): Sentence = defaultInstance.mergeFrom(stream)
      def parseDelimitedFrom(stream: java.io.InputStream): Option[Sentence] = defaultInstance.mergeDelimitedFromStream(stream)

      val TOKENS_FIELD_NUMBER = 1
      val MENTIONS_FIELD_NUMBER = 2
      val DEPTREE_FIELD_NUMBER = 3

      def newBuilder = defaultInstance.newBuilderForType
      def newBuilder(prototype: Sentence) = defaultInstance.mergeFrom(prototype)

    }
    final case class Token (
                             `word`: String = "",
                             `tag`: Option[String] = None,
                             `ner`: Option[String] = None
                             ) extends com.google.protobuf.GeneratedMessageLite
    with com.google.protobuf.MessageLite.Builder
    with net.sandrogrzicic.scalabuff.Message[Token]
    with net.sandrogrzicic.scalabuff.Parser[Token] {

      def setTag(_f: String) = copy(`tag` = Some(_f))
      def setNer(_f: String) = copy(`ner` = Some(_f))

      def clearTag = copy(`tag` = None)
      def clearNer = copy(`ner` = None)

      def writeTo(output: com.google.protobuf.CodedOutputStream) {
        output.writeString(1, `word`)
        if (`tag`.isDefined) output.writeString(2, `tag`.get)
        if (`ner`.isDefined) output.writeString(3, `ner`.get)
      }

      def getSerializedSize = {
        import com.google.protobuf.CodedOutputStream._
        var __size = 0
        __size += computeStringSize(1, `word`)
        if (`tag`.isDefined) __size += computeStringSize(2, `tag`.get)
        if (`ner`.isDefined) __size += computeStringSize(3, `ner`.get)

        __size
      }

      def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Token = {
        import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
        var __word: String = ""
        var __tag: Option[String] = `tag`
        var __ner: Option[String] = `ner`

        def __newMerged = Token(
          __word,
          __tag,
          __ner
        )
        while (true) in.readTag match {
          case 0 => return __newMerged
          case 10 => __word = in.readString()
          case 18 => __tag = Some(in.readString())
          case 26 => __ner = Some(in.readString())
          case default => if (!in.skipField(default)) return __newMerged
        }
        null
      }

      def mergeFrom(m: Token) = {
        Token(
          m.`word`,
          m.`tag`.orElse(`tag`),
          m.`ner`.orElse(`ner`)
        )
      }

      def getDefaultInstanceForType = Token.defaultInstance
      def clear = getDefaultInstanceForType
      def isInitialized = true
      def build = this
      def buildPartial = this
      def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
      override def getParserForType = this
      def newBuilderForType = getDefaultInstanceForType
      def toBuilder = this
      def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
    }

    object Token {
      @scala.beans.BeanProperty val defaultInstance = new Token()

      def parseFrom(data: Array[Byte]): Token = defaultInstance.mergeFrom(data)
      def parseFrom(data: Array[Byte], offset: Int, length: Int): Token = defaultInstance.mergeFrom(data, offset, length)
      def parseFrom(byteString: com.google.protobuf.ByteString): Token = defaultInstance.mergeFrom(byteString)
      def parseFrom(stream: java.io.InputStream): Token = defaultInstance.mergeFrom(stream)
      def parseDelimitedFrom(stream: java.io.InputStream): Option[Token] = defaultInstance.mergeDelimitedFromStream(stream)

      val WORD_FIELD_NUMBER = 1
      val TAG_FIELD_NUMBER = 2
      val NER_FIELD_NUMBER = 3

      def newBuilder = defaultInstance.newBuilderForType
      def newBuilder(prototype: Token) = defaultInstance.mergeFrom(prototype)

    }
    final case class Mention (
                               `id`: Int = 0,
                               `entityGuid`: Option[String] = None,
                               `from`: Int = 0,
                               `to`: Int = 0,
                               `label`: String = ""
                               ) extends com.google.protobuf.GeneratedMessageLite
    with com.google.protobuf.MessageLite.Builder
    with net.sandrogrzicic.scalabuff.Message[Mention]
    with net.sandrogrzicic.scalabuff.Parser[Mention] {

      def setEntityGuid(_f: String) = copy(`entityGuid` = Some(_f))

      def clearEntityGuid = copy(`entityGuid` = None)

      def writeTo(output: com.google.protobuf.CodedOutputStream) {
        output.writeInt32(1, `id`)
        if (`entityGuid`.isDefined) output.writeString(2, `entityGuid`.get)
        output.writeInt32(3, `from`)
        output.writeInt32(4, `to`)
        output.writeString(5, `label`)
      }

      def getSerializedSize = {
        import com.google.protobuf.CodedOutputStream._
        var __size = 0
        __size += computeInt32Size(1, `id`)
        if (`entityGuid`.isDefined) __size += computeStringSize(2, `entityGuid`.get)
        __size += computeInt32Size(3, `from`)
        __size += computeInt32Size(4, `to`)
        __size += computeStringSize(5, `label`)

        __size
      }

      def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Mention = {
        import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
        var __id: Int = 0
        var __entityGuid: Option[String] = `entityGuid`
        var __from: Int = 0
        var __to: Int = 0
        var __label: String = ""

        def __newMerged = Mention(
          __id,
          __entityGuid,
          __from,
          __to,
          __label
        )
        while (true) in.readTag match {
          case 0 => return __newMerged
          case 8 => __id = in.readInt32()
          case 18 => __entityGuid = Some(in.readString())
          case 24 => __from = in.readInt32()
          case 32 => __to = in.readInt32()
          case 42 => __label = in.readString()
          case default => if (!in.skipField(default)) return __newMerged
        }
        null
      }

      def mergeFrom(m: Mention) = {
        Mention(
          m.`id`,
          m.`entityGuid`.orElse(`entityGuid`),
          m.`from`,
          m.`to`,
          m.`label`
        )
      }

      def getDefaultInstanceForType = Mention.defaultInstance
      def clear = getDefaultInstanceForType
      def isInitialized = true
      def build = this
      def buildPartial = this
      def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
      override def getParserForType = this
      def newBuilderForType = getDefaultInstanceForType
      def toBuilder = this
      def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
    }

    object Mention {
      @scala.beans.BeanProperty val defaultInstance = new Mention()

      def parseFrom(data: Array[Byte]): Mention = defaultInstance.mergeFrom(data)
      def parseFrom(data: Array[Byte], offset: Int, length: Int): Mention = defaultInstance.mergeFrom(data, offset, length)
      def parseFrom(byteString: com.google.protobuf.ByteString): Mention = defaultInstance.mergeFrom(byteString)
      def parseFrom(stream: java.io.InputStream): Mention = defaultInstance.mergeFrom(stream)
      def parseDelimitedFrom(stream: java.io.InputStream): Option[Mention] = defaultInstance.mergeDelimitedFromStream(stream)

      val ID_FIELD_NUMBER = 1
      val ENTITYGUID_FIELD_NUMBER = 2
      val FROM_FIELD_NUMBER = 3
      val TO_FIELD_NUMBER = 4
      val LABEL_FIELD_NUMBER = 5

      def newBuilder = defaultInstance.newBuilderForType
      def newBuilder(prototype: Mention) = defaultInstance.mergeFrom(prototype)

    }
    final case class DepTree (
                               `root`: Int = 0,
                               `head`: scala.collection.immutable.Seq[Int] = Vector.empty[Int],
                               `relType`: scala.collection.immutable.Seq[String] = Vector.empty[String]
                               ) extends com.google.protobuf.GeneratedMessageLite
    with com.google.protobuf.MessageLite.Builder
    with net.sandrogrzicic.scalabuff.Message[DepTree]
    with net.sandrogrzicic.scalabuff.Parser[DepTree] {

      def setHead(_i: Int, _v: Int) = copy(`head` = `head`.updated(_i, _v))
      def addHead(_f: Int) = copy(`head` = `head` :+ _f)
      def addAllHead(_f: Int*) = copy(`head` = `head` ++ _f)
      def addAllHead(_f: TraversableOnce[Int]) = copy(`head` = `head` ++ _f)
      def setRelType(_i: Int, _v: String) = copy(`relType` = `relType`.updated(_i, _v))
      def addRelType(_f: String) = copy(`relType` = `relType` :+ _f)
      def addAllRelType(_f: String*) = copy(`relType` = `relType` ++ _f)
      def addAllRelType(_f: TraversableOnce[String]) = copy(`relType` = `relType` ++ _f)

      def clearHead = copy(`head` = Vector.empty[Int])
      def clearRelType = copy(`relType` = Vector.empty[String])

      def writeTo(output: com.google.protobuf.CodedOutputStream) {
        output.writeInt32(1, `root`)
        for (_v <- `head`) output.writeInt32(2, _v)
        for (_v <- `relType`) output.writeString(3, _v)
      }

      def getSerializedSize = {
        import com.google.protobuf.CodedOutputStream._
        var __size = 0
        __size += computeInt32Size(1, `root`)
        for (_v <- `head`) __size += computeInt32Size(2, _v)
        for (_v <- `relType`) __size += computeStringSize(3, _v)

        __size
      }

      def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): DepTree = {
        import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
        var __root: Int = 0
        val __head: scala.collection.mutable.Buffer[Int] = `head`.toBuffer
        val __relType: scala.collection.mutable.Buffer[String] = `relType`.toBuffer

        def __newMerged = DepTree(
          __root,
          Vector(__head: _*),
          Vector(__relType: _*)
        )
        while (true) in.readTag match {
          case 0 => return __newMerged
          case 8 => __root = in.readInt32()
          case 16 => __head += in.readInt32()
          case 18 =>
            val length = in.readRawVarint32()
            val limit = in.pushLimit(length)
            while (in.getBytesUntilLimit() > 0) {
              __head += in.readInt32()
            }
            in.popLimit(limit)
          case 26 => __relType += in.readString()
          case default => if (!in.skipField(default)) return __newMerged
        }
        null
      }

      def mergeFrom(m: DepTree) = {
        DepTree(
          m.`root`,
          `head` ++ m.`head`,
          `relType` ++ m.`relType`
        )
      }

      def getDefaultInstanceForType = DepTree.defaultInstance
      def clear = getDefaultInstanceForType
      def isInitialized = true
      def build = this
      def buildPartial = this
      def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
      override def getParserForType = this
      def newBuilderForType = getDefaultInstanceForType
      def toBuilder = this
      def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
    }

    object DepTree {
      @scala.beans.BeanProperty val defaultInstance = new DepTree()

      def parseFrom(data: Array[Byte]): DepTree = defaultInstance.mergeFrom(data)
      def parseFrom(data: Array[Byte], offset: Int, length: Int): DepTree = defaultInstance.mergeFrom(data, offset, length)
      def parseFrom(byteString: com.google.protobuf.ByteString): DepTree = defaultInstance.mergeFrom(byteString)
      def parseFrom(stream: java.io.InputStream): DepTree = defaultInstance.mergeFrom(stream)
      def parseDelimitedFrom(stream: java.io.InputStream): Option[DepTree] = defaultInstance.mergeDelimitedFromStream(stream)

      val ROOT_FIELD_NUMBER = 1
      val HEAD_FIELD_NUMBER = 2
      val RELTYPE_FIELD_NUMBER = 3

      def newBuilder = defaultInstance.newBuilderForType
      def newBuilder(prototype: DepTree) = defaultInstance.mergeFrom(prototype)

    }
    final case class RelationMention (
                                       `id`: Int = 0,
                                       `source`: Int = 0,
                                       `dest`: Int = 0,
                                       `label`: String = ""
                                       ) extends com.google.protobuf.GeneratedMessageLite
    with com.google.protobuf.MessageLite.Builder
    with net.sandrogrzicic.scalabuff.Message[RelationMention]
    with net.sandrogrzicic.scalabuff.Parser[RelationMention] {



      def writeTo(output: com.google.protobuf.CodedOutputStream) {
        output.writeInt32(1, `id`)
        output.writeInt32(2, `source`)
        output.writeInt32(3, `dest`)
        output.writeString(4, `label`)
      }

      def getSerializedSize = {
        import com.google.protobuf.CodedOutputStream._
        var __size = 0
        __size += computeInt32Size(1, `id`)
        __size += computeInt32Size(2, `source`)
        __size += computeInt32Size(3, `dest`)
        __size += computeStringSize(4, `label`)

        __size
      }

      def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): RelationMention = {
        import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
        var __id: Int = 0
        var __source: Int = 0
        var __dest: Int = 0
        var __label: String = ""

        def __newMerged = RelationMention(
          __id,
          __source,
          __dest,
          __label
        )
        while (true) in.readTag match {
          case 0 => return __newMerged
          case 8 => __id = in.readInt32()
          case 16 => __source = in.readInt32()
          case 24 => __dest = in.readInt32()
          case 34 => __label = in.readString()
          case default => if (!in.skipField(default)) return __newMerged
        }
        null
      }

      def mergeFrom(m: RelationMention) = {
        RelationMention(
          m.`id`,
          m.`source`,
          m.`dest`,
          m.`label`
        )
      }

      def getDefaultInstanceForType = RelationMention.defaultInstance
      def clear = getDefaultInstanceForType
      def isInitialized = true
      def build = this
      def buildPartial = this
      def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
      override def getParserForType = this
      def newBuilderForType = getDefaultInstanceForType
      def toBuilder = this
      def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
    }

    object RelationMention {
      @scala.beans.BeanProperty val defaultInstance = new RelationMention()

      def parseFrom(data: Array[Byte]): RelationMention = defaultInstance.mergeFrom(data)
      def parseFrom(data: Array[Byte], offset: Int, length: Int): RelationMention = defaultInstance.mergeFrom(data, offset, length)
      def parseFrom(byteString: com.google.protobuf.ByteString): RelationMention = defaultInstance.mergeFrom(byteString)
      def parseFrom(stream: java.io.InputStream): RelationMention = defaultInstance.mergeFrom(stream)
      def parseDelimitedFrom(stream: java.io.InputStream): Option[RelationMention] = defaultInstance.mergeDelimitedFromStream(stream)

      val ID_FIELD_NUMBER = 1
      val SOURCE_FIELD_NUMBER = 2
      val DEST_FIELD_NUMBER = 3
      val LABEL_FIELD_NUMBER = 4

      def newBuilder = defaultInstance.newBuilderForType
      def newBuilder(prototype: RelationMention) = defaultInstance.mergeFrom(prototype)

    }
  }
  final case class Riedel2010Relation (
                                        `sourceGuid`: String = "",
                                        `destGuid`: String = "",
                                        `relType`: String = "",
                                        `mention`: scala.collection.immutable.Seq[Riedel2010Relation.RelationMentionRef] = Vector.empty[Riedel2010Relation.RelationMentionRef]
                                        ) extends com.google.protobuf.GeneratedMessageLite
  with com.google.protobuf.MessageLite.Builder
  with net.sandrogrzicic.scalabuff.Message[Riedel2010Relation]
  with net.sandrogrzicic.scalabuff.Parser[Riedel2010Relation] {

    def setMention(_i: Int, _v: Riedel2010Relation.RelationMentionRef) = copy(`mention` = `mention`.updated(_i, _v))
    def addMention(_f: Riedel2010Relation.RelationMentionRef) = copy(`mention` = `mention` :+ _f)
    def addAllMention(_f: Riedel2010Relation.RelationMentionRef*) = copy(`mention` = `mention` ++ _f)
    def addAllMention(_f: TraversableOnce[Riedel2010Relation.RelationMentionRef]) = copy(`mention` = `mention` ++ _f)

    def clearMention = copy(`mention` = Vector.empty[Riedel2010Relation.RelationMentionRef])

    def writeTo(output: com.google.protobuf.CodedOutputStream) {
      output.writeString(1, `sourceGuid`)
      output.writeString(2, `destGuid`)
      output.writeString(3, `relType`)
      for (_v <- `mention`) output.writeMessage(4, _v)
    }

    def getSerializedSize = {
      import com.google.protobuf.CodedOutputStream._
      var __size = 0
      __size += computeStringSize(1, `sourceGuid`)
      __size += computeStringSize(2, `destGuid`)
      __size += computeStringSize(3, `relType`)
      for (_v <- `mention`) __size += computeMessageSize(4, _v)

      __size
    }

    def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Riedel2010Relation = {
      import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
      var __sourceGuid: String = ""
      var __destGuid: String = ""
      var __relType: String = ""
      val __mention: scala.collection.mutable.Buffer[Riedel2010Relation.RelationMentionRef] = `mention`.toBuffer

      def __newMerged = Riedel2010Relation(
        __sourceGuid,
        __destGuid,
        __relType,
        Vector(__mention: _*)
      )
      while (true) in.readTag match {
        case 0 => return __newMerged
        case 10 => __sourceGuid = in.readString()
        case 18 => __destGuid = in.readString()
        case 26 => __relType = in.readString()
        case 34 => __mention += readMessage[Riedel2010Relation.RelationMentionRef](in, Riedel2010Relation.RelationMentionRef.defaultInstance, _emptyRegistry)
        case default => if (!in.skipField(default)) return __newMerged
      }
      null
    }

    def mergeFrom(m: Riedel2010Relation) = {
      Riedel2010Relation(
        m.`sourceGuid`,
        m.`destGuid`,
        m.`relType`,
        `mention` ++ m.`mention`
      )
    }

    def getDefaultInstanceForType = Riedel2010Relation.defaultInstance
    def clear = getDefaultInstanceForType
    def isInitialized = true
    def build = this
    def buildPartial = this
    def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
    override def getParserForType = this
    def newBuilderForType = getDefaultInstanceForType
    def toBuilder = this
    def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
  }

  object Riedel2010Relation {
    @scala.beans.BeanProperty val defaultInstance = new Riedel2010Relation()

    def parseFrom(data: Array[Byte]): Riedel2010Relation = defaultInstance.mergeFrom(data)
    def parseFrom(data: Array[Byte], offset: Int, length: Int): Riedel2010Relation = defaultInstance.mergeFrom(data, offset, length)
    def parseFrom(byteString: com.google.protobuf.ByteString): Riedel2010Relation = defaultInstance.mergeFrom(byteString)
    def parseFrom(stream: java.io.InputStream): Riedel2010Relation = defaultInstance.mergeFrom(stream)
    def parseDelimitedFrom(stream: java.io.InputStream): Option[Riedel2010Relation] = defaultInstance.mergeDelimitedFromStream(stream)

    val SOURCEGUID_FIELD_NUMBER = 1
    val DESTGUID_FIELD_NUMBER = 2
    val RELTYPE_FIELD_NUMBER = 3
    val MENTION_FIELD_NUMBER = 4

    def newBuilder = defaultInstance.newBuilderForType
    def newBuilder(prototype: Riedel2010Relation) = defaultInstance.mergeFrom(prototype)

    final case class RelationMentionRef (
                                          `filename`: String = "",
                                          `sourceId`: Int = 0,
                                          `destId`: Int = 0,
                                          `feature`: scala.collection.immutable.Seq[String] = Vector.empty[String],
                                          `sentence`: Option[String] = None
                                          ) extends com.google.protobuf.GeneratedMessageLite
    with com.google.protobuf.MessageLite.Builder
    with net.sandrogrzicic.scalabuff.Message[RelationMentionRef]
    with net.sandrogrzicic.scalabuff.Parser[RelationMentionRef] {

      def setFeature(_i: Int, _v: String) = copy(`feature` = `feature`.updated(_i, _v))
      def addFeature(_f: String) = copy(`feature` = `feature` :+ _f)
      def addAllFeature(_f: String*) = copy(`feature` = `feature` ++ _f)
      def addAllFeature(_f: TraversableOnce[String]) = copy(`feature` = `feature` ++ _f)
      def setSentence(_f: String) = copy(`sentence` = Some(_f))

      def clearFeature = copy(`feature` = Vector.empty[String])
      def clearSentence = copy(`sentence` = None)

      def writeTo(output: com.google.protobuf.CodedOutputStream) {
        output.writeString(1, `filename`)
        output.writeInt32(2, `sourceId`)
        output.writeInt32(3, `destId`)
        for (_v <- `feature`) output.writeString(4, _v)
        if (`sentence`.isDefined) output.writeString(5, `sentence`.get)
      }

      def getSerializedSize = {
        import com.google.protobuf.CodedOutputStream._
        var __size = 0
        __size += computeStringSize(1, `filename`)
        __size += computeInt32Size(2, `sourceId`)
        __size += computeInt32Size(3, `destId`)
        for (_v <- `feature`) __size += computeStringSize(4, _v)
        if (`sentence`.isDefined) __size += computeStringSize(5, `sentence`.get)

        __size
      }

      def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): RelationMentionRef = {
        import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
        var __filename: String = ""
        var __sourceId: Int = 0
        var __destId: Int = 0
        val __feature: scala.collection.mutable.Buffer[String] = `feature`.toBuffer
        var __sentence: Option[String] = `sentence`

        def __newMerged = RelationMentionRef(
          __filename,
          __sourceId,
          __destId,
          Vector(__feature: _*),
          __sentence
        )
        while (true) in.readTag match {
          case 0 => return __newMerged
          case 10 => __filename = in.readString()
          case 16 => __sourceId = in.readInt32()
          case 24 => __destId = in.readInt32()
          case 34 => __feature += in.readString()
          case 42 => __sentence = Some(in.readString())
          case default => if (!in.skipField(default)) return __newMerged
        }
        null
      }

      def mergeFrom(m: RelationMentionRef) = {
        RelationMentionRef(
          m.`filename`,
          m.`sourceId`,
          m.`destId`,
          `feature` ++ m.`feature`,
          m.`sentence`.orElse(`sentence`)
        )
      }

      def getDefaultInstanceForType = RelationMentionRef.defaultInstance
      def clear = getDefaultInstanceForType
      def isInitialized = true
      def build = this
      def buildPartial = this
      def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
      override def getParserForType = this
      def newBuilderForType = getDefaultInstanceForType
      def toBuilder = this
      def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
    }

    object RelationMentionRef {
      @scala.beans.BeanProperty val defaultInstance = new RelationMentionRef()

      def parseFrom(data: Array[Byte]): RelationMentionRef = defaultInstance.mergeFrom(data)
      def parseFrom(data: Array[Byte], offset: Int, length: Int): RelationMentionRef = defaultInstance.mergeFrom(data, offset, length)
      def parseFrom(byteString: com.google.protobuf.ByteString): RelationMentionRef = defaultInstance.mergeFrom(byteString)
      def parseFrom(stream: java.io.InputStream): RelationMentionRef = defaultInstance.mergeFrom(stream)
      def parseDelimitedFrom(stream: java.io.InputStream): Option[RelationMentionRef] = defaultInstance.mergeDelimitedFromStream(stream)

      val FILENAME_FIELD_NUMBER = 1
      val SOURCEID_FIELD_NUMBER = 2
      val DESTID_FIELD_NUMBER = 3
      val FEATURE_FIELD_NUMBER = 4
      val SENTENCE_FIELD_NUMBER = 5

      def newBuilder = defaultInstance.newBuilderForType
      def newBuilder(prototype: RelationMentionRef) = defaultInstance.mergeFrom(prototype)

    }
  }
  final case class Riedel2010Entity (
                                      `guid`: String = "",
                                      `name`: Option[String] = None,
                                      `type`: Option[String] = None,
                                      `pred`: Option[String] = None,
                                      `mention`: scala.collection.immutable.Seq[Riedel2010Entity.EntityMentionRef] = Vector.empty[Riedel2010Entity.EntityMentionRef]
                                      ) extends com.google.protobuf.GeneratedMessageLite
  with com.google.protobuf.MessageLite.Builder
  with net.sandrogrzicic.scalabuff.Message[Riedel2010Entity]
  with net.sandrogrzicic.scalabuff.Parser[Riedel2010Entity] {

    def setName(_f: String) = copy(`name` = Some(_f))
    def setType(_f: String) = copy(`type` = Some(_f))
    def setPred(_f: String) = copy(`pred` = Some(_f))
    def setMention(_i: Int, _v: Riedel2010Entity.EntityMentionRef) = copy(`mention` = `mention`.updated(_i, _v))
    def addMention(_f: Riedel2010Entity.EntityMentionRef) = copy(`mention` = `mention` :+ _f)
    def addAllMention(_f: Riedel2010Entity.EntityMentionRef*) = copy(`mention` = `mention` ++ _f)
    def addAllMention(_f: TraversableOnce[Riedel2010Entity.EntityMentionRef]) = copy(`mention` = `mention` ++ _f)

    def clearName = copy(`name` = None)
    def clearType = copy(`type` = None)
    def clearPred = copy(`pred` = None)
    def clearMention = copy(`mention` = Vector.empty[Riedel2010Entity.EntityMentionRef])

    def writeTo(output: com.google.protobuf.CodedOutputStream) {
      output.writeString(1, `guid`)
      if (`name`.isDefined) output.writeString(2, `name`.get)
      if (`type`.isDefined) output.writeString(3, `type`.get)
      if (`pred`.isDefined) output.writeString(4, `pred`.get)
      for (_v <- `mention`) output.writeMessage(5, _v)
    }

    def getSerializedSize = {
      import com.google.protobuf.CodedOutputStream._
      var __size = 0
      __size += computeStringSize(1, `guid`)
      if (`name`.isDefined) __size += computeStringSize(2, `name`.get)
      if (`type`.isDefined) __size += computeStringSize(3, `type`.get)
      if (`pred`.isDefined) __size += computeStringSize(4, `pred`.get)
      for (_v <- `mention`) __size += computeMessageSize(5, _v)

      __size
    }

    def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): Riedel2010Entity = {
      import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
      var __guid: String = ""
      var __name: Option[String] = `name`
      var __type: Option[String] = `type`
      var __pred: Option[String] = `pred`
      val __mention: scala.collection.mutable.Buffer[Riedel2010Entity.EntityMentionRef] = `mention`.toBuffer

      def __newMerged = Riedel2010Entity(
        __guid,
        __name,
        __type,
        __pred,
        Vector(__mention: _*)
      )
      while (true) in.readTag match {
        case 0 => return __newMerged
        case 10 => __guid = in.readString()
        case 18 => __name = Some(in.readString())
        case 26 => __type = Some(in.readString())
        case 34 => __pred = Some(in.readString())
        case 42 => __mention += readMessage[Riedel2010Entity.EntityMentionRef](in, Riedel2010Entity.EntityMentionRef.defaultInstance, _emptyRegistry)
        case default => if (!in.skipField(default)) return __newMerged
      }
      null
    }

    def mergeFrom(m: Riedel2010Entity) = {
      Riedel2010Entity(
        m.`guid`,
        m.`name`.orElse(`name`),
        m.`type`.orElse(`type`),
        m.`pred`.orElse(`pred`),
        `mention` ++ m.`mention`
      )
    }

    def getDefaultInstanceForType = Riedel2010Entity.defaultInstance
    def clear = getDefaultInstanceForType
    def isInitialized = true
    def build = this
    def buildPartial = this
    def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
    override def getParserForType = this
    def newBuilderForType = getDefaultInstanceForType
    def toBuilder = this
    def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
  }

  object Riedel2010Entity {
    @scala.beans.BeanProperty val defaultInstance = new Riedel2010Entity()

    def parseFrom(data: Array[Byte]): Riedel2010Entity = defaultInstance.mergeFrom(data)
    def parseFrom(data: Array[Byte], offset: Int, length: Int): Riedel2010Entity = defaultInstance.mergeFrom(data, offset, length)
    def parseFrom(byteString: com.google.protobuf.ByteString): Riedel2010Entity = defaultInstance.mergeFrom(byteString)
    def parseFrom(stream: java.io.InputStream): Riedel2010Entity = defaultInstance.mergeFrom(stream)
    def parseDelimitedFrom(stream: java.io.InputStream): Option[Riedel2010Entity] = defaultInstance.mergeDelimitedFromStream(stream)

    val GUID_FIELD_NUMBER = 1
    val NAME_FIELD_NUMBER = 2
    val TYPE_FIELD_NUMBER = 3
    val PRED_FIELD_NUMBER = 4
    val MENTION_FIELD_NUMBER = 5

    def newBuilder = defaultInstance.newBuilderForType
    def newBuilder(prototype: Riedel2010Entity) = defaultInstance.mergeFrom(prototype)

    final case class EntityMentionRef (
                                        `filename`: String = "",
                                        `id`: Int = 0,
                                        `feature`: scala.collection.immutable.Seq[String] = Vector.empty[String]
                                        ) extends com.google.protobuf.GeneratedMessageLite
    with com.google.protobuf.MessageLite.Builder
    with net.sandrogrzicic.scalabuff.Message[EntityMentionRef]
    with net.sandrogrzicic.scalabuff.Parser[EntityMentionRef] {

      def setFeature(_i: Int, _v: String) = copy(`feature` = `feature`.updated(_i, _v))
      def addFeature(_f: String) = copy(`feature` = `feature` :+ _f)
      def addAllFeature(_f: String*) = copy(`feature` = `feature` ++ _f)
      def addAllFeature(_f: TraversableOnce[String]) = copy(`feature` = `feature` ++ _f)

      def clearFeature = copy(`feature` = Vector.empty[String])

      def writeTo(output: com.google.protobuf.CodedOutputStream) {
        output.writeString(1, `filename`)
        output.writeInt32(2, `id`)
        for (_v <- `feature`) output.writeString(3, _v)
      }

      def getSerializedSize = {
        import com.google.protobuf.CodedOutputStream._
        var __size = 0
        __size += computeStringSize(1, `filename`)
        __size += computeInt32Size(2, `id`)
        for (_v <- `feature`) __size += computeStringSize(3, _v)

        __size
      }

      def mergeFrom(in: com.google.protobuf.CodedInputStream, extensionRegistry: com.google.protobuf.ExtensionRegistryLite): EntityMentionRef = {
        import com.google.protobuf.ExtensionRegistryLite.{getEmptyRegistry => _emptyRegistry}
        var __filename: String = ""
        var __id: Int = 0
        val __feature: scala.collection.mutable.Buffer[String] = `feature`.toBuffer

        def __newMerged = EntityMentionRef(
          __filename,
          __id,
          Vector(__feature: _*)
        )
        while (true) in.readTag match {
          case 0 => return __newMerged
          case 10 => __filename = in.readString()
          case 16 => __id = in.readInt32()
          case 26 => __feature += in.readString()
          case default => if (!in.skipField(default)) return __newMerged
        }
        null
      }

      def mergeFrom(m: EntityMentionRef) = {
        EntityMentionRef(
          m.`filename`,
          m.`id`,
          `feature` ++ m.`feature`
        )
      }

      def getDefaultInstanceForType = EntityMentionRef.defaultInstance
      def clear = getDefaultInstanceForType
      def isInitialized = true
      def build = this
      def buildPartial = this
      def parsePartialFrom(cis: com.google.protobuf.CodedInputStream, er: com.google.protobuf.ExtensionRegistryLite) = mergeFrom(cis, er)
      override def getParserForType = this
      def newBuilderForType = getDefaultInstanceForType
      def toBuilder = this
      def toJson(indent: Int = 0): String = "ScalaBuff JSON generation not enabled. Use --generate_json_method to enable."
    }

    object EntityMentionRef {
      @scala.beans.BeanProperty val defaultInstance = new EntityMentionRef()

      def parseFrom(data: Array[Byte]): EntityMentionRef = defaultInstance.mergeFrom(data)
      def parseFrom(data: Array[Byte], offset: Int, length: Int): EntityMentionRef = defaultInstance.mergeFrom(data, offset, length)
      def parseFrom(byteString: com.google.protobuf.ByteString): EntityMentionRef = defaultInstance.mergeFrom(byteString)
      def parseFrom(stream: java.io.InputStream): EntityMentionRef = defaultInstance.mergeFrom(stream)
      def parseDelimitedFrom(stream: java.io.InputStream): Option[EntityMentionRef] = defaultInstance.mergeDelimitedFromStream(stream)

      val FILENAME_FIELD_NUMBER = 1
      val ID_FIELD_NUMBER = 2
      val FEATURE_FIELD_NUMBER = 3

      def newBuilder = defaultInstance.newBuilderForType
      def newBuilder(prototype: EntityMentionRef) = defaultInstance.mergeFrom(prototype)

    }
  }

  object Riedel2010DocumentProtos {
    def registerAllExtensions(registry: com.google.protobuf.ExtensionRegistryLite) {
    }

    private val fromBinaryHintMap = collection.immutable.HashMap[String, Array[Byte] ⇒ com.google.protobuf.GeneratedMessageLite](
      "Document" -> (bytes ⇒ Riedel2010Document.parseFrom(bytes)),
      "Relation" -> (bytes ⇒ Riedel2010Relation.parseFrom(bytes)),
      "Entity" -> (bytes ⇒ Riedel2010Entity.parseFrom(bytes))
    )

    def deserializePayload(payload: Array[Byte], payloadType: String): com.google.protobuf.GeneratedMessageLite = {
      fromBinaryHintMap.get(payloadType) match {
        case Some(f) ⇒ f(payload)
        case None    ⇒ throw new IllegalArgumentException(s"unimplemented deserialization of message payload of type [${payloadType}]")
      }
    }
  }

}