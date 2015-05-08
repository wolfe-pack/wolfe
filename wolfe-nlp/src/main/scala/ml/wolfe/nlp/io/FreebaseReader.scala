package ml.wolfe.nlp.io

import com.mongodb.casbah.Imports._
import java.io.{File, FileWriter}

import scala.util.matching.Regex

/**
 * Created by narad on 9/11/14.
 */

// To use the FreebaseReader class you must first start a MongoDB process in the environment.
// A simple way of doing this is the command: mongod --dbpath <path-to-DB-cache-directory>

class FreebaseReader(collection: MongoCollection) {
  //filename: String, port: Int = 27017, useExisting: Boolean = true) {
//  var collection =  mongoFromTriples //null.asInstanceOf[MongoCollection]

  def getMIDs: Iterator[String] = {
    val q = new MongoDBObject("mid" $exists true)
    (collection find q).map { _.get("mid").asInstanceOf[String] }
  }

  def getCandidateMIDs(str: String): IndexedSeq[String] = {
    val name = str.replaceAll(" ", "_")
    (collection find MongoDBObject("attribute" -> "title", "arg2" -> name)).flatMap { m =>
      if (m.contains("arg1")) Some(m.get("arg1").asInstanceOf[String])
      else None
    }.toIndexedSeq
  }

  def getRelationFromNames(str1: String, str2: String, desc: String = "", undirected: Boolean = false): Option[String] = {
    val mids1 = getCandidateMIDs(str1)
    val mids2 = getCandidateMIDs(str2)
    if (mids1.isEmpty || mids2.isEmpty) return None
    // Do some selection from candidate MIDs - this could be a text similarity measure, random selection, or just first match
    val mid1 = if (desc.size > 1) rankMIDs(mids1, desc).head else mids1.head
    val mid2 = if (desc.size > 1) rankMIDs(mids2, desc).head else mids2.head
    getRelationFromMIDs(mid1, mid2, undirected)
  }

  def getRelationFromMIDs(mid1: String, mid2: String, undirected: Boolean = false): Option[String] = {
    // Look for relations (attributes) shared by both MIDs
    val m1 = collection findOne MongoDBObject("arg1" -> mid1, "arg2" -> mid2)
    m1 match {
      case Some(row) => return Some(row.get("attribute").asInstanceOf[String])
      case rev if undirected => {
        // No match found so try the opposite arg1/arg2s:
        val m2 = collection findOne MongoDBObject("arg1" -> mid2, "arg2" -> mid1)
        m2 match {
          case Some(row) => return Some(row.get("attribute").asInstanceOf[String])
          case _=> None
        }
      }
      case _=> None
    }
  }

  def rankMIDs(mids: Seq[String], desc: String): Seq[String] = {
    mids.map { m =>
      getDescription(m) match {
        case Some(d) => (m, similarity(d, desc))
        case _ => (m, 0.0)
      }
    }.sortBy(-1.0 * _._2).map(_._1)
  }

  def similarity(str1: String, str2: String): Double = {
    val tokens1 = str1.split(" ").distinct
    val tokens2 = str2.split(" ").distinct
    tokens1.map(t => if (tokens2.contains(t)) 1.0 else 0.0).sum
  }

  def getAttribute(mid: String, attribute: String): Option[String] = {
    collection findOne MongoDBObject("arg1" -> mid, "attribute" -> attribute) match {
      case Some(record) => Some(record.get("arg2").toString)
      case _ => None
    }
  }

  def getAllOfAttribute(mid: String, attribute: String): Seq[String] = {
    val query = MongoDBObject("arg1" -> mid, "attribute" -> attribute)
    (collection find query).flatMap { _ match {
      case r if r.containsField("arg2") => Some(r.get("arg2").toString)
      case _=> None
      }
    }.toSeq
  }

  def attributesOf(mid: String): Map[String, String] = {
    val query = MongoDBObject("arg1" -> mid)
    (collection find query).map { m =>
      val t1 = m.getOrElse("attribute", "None").toString
      val t2 = m.getOrElse("arg2", "None").toString
      t1 -> t2
    }.filter(t => !(t._1 == "None" && t._2 == "None")).toMap
  }

  def parentsOf(mid: String): Map[String, String] = {
    val query = MongoDBObject("arg2" -> mid)
    (collection find query).map { m =>
      val t1 = m.getOrElse("attribute", "None").toString
      val t2 = m.getOrElse("arg1", "None").toString
      t1 -> t2
    }.filter(t => !(t._1 == "None" && t._2 == "None")).toMap
  }

  def parentsOf(mids: Seq[String]): Map[String, String] = {
    // val nor = $nor { ("foo" $gte 15 $lt 35 $ne 16) + ("x" -> "y") }
    val query = "array" $all mids.map("arg2" -> _)

 //   val query = MongoDBObject("arg2 : { $in : [ %s ]}".format(mids.mkString(", ")))
    (collection find query).map { m =>
      val t1 = m.getOrElse("attribute", "None").toString
      val t2 = m.getOrElse("arg1", "None").toString
      t1 -> t2
    }.filter(t => !(t._1 == "None" && t._2 == "None")).toMap
  }

  def getName(mid: String): Option[String] = getAttribute(mid, "title")

  def getDescription(mid: String): Option[String] = getAttribute(mid, "text")

  def test() {
    // Set collection to the test collection
 //   collection = testCollection
    // Query
    println("Attributes of m1: " + attributesOf("m1"))
    println("MID of 'Barack Obama': " + getCandidateMIDs("Barack Obama"))
    println("MID of OOV: " + getCandidateMIDs("Barack"))
    println("Relation between Barack Obama and US? " + getRelationFromNames("Barack Obama", "United States of America"))
    println("Relation between Michelle and Barack? " + getRelationFromNames("Michelle Obama", "Barack Obama"))
    println("Relation between Michelle and US? " + getRelationFromNames("Michelle Obama", "United States of America"))

    println(parentsOf("m.02y4yg"))
    println
    println(attributesOf("m.0h_c7_2"))
    println
    println(rankByShare(Seq("m.02y4yg", "m.0ds6ccp", "m.0114czf1", "2012-01-13", "m.0cd72h")))

  }

  def rankByShare(mids: Seq[String]): Map[String, Int] = {
    for (m <- mids) {
      println(m + ":")
      println(parentsOf(m).map(p => "\t" + p._1 + " --> " + p._2).mkString("\n"))
    }
    println("-------")
    val allParents = mids.map(parentsOf(_)).toArray.flatten
    println(allParents.sortBy(_.toString).mkString("\n"))
    allParents.groupBy(_._2).map(p => p._1 -> p._2.size)
  }



  def testCollection: MongoCollection = {
    val mongoClient = MongoClient("localhost", 27017)
    mongoClient.dropDatabase("TEST")
    val db = mongoClient("TEST")
    val coll = db("TEST")
    // Set indexes
    coll.ensureIndex("mid")
    coll.ensureIndex("arg1")
    coll.ensureIndex("arg2")
    coll.ensureIndex("type")
    coll.ensureIndex("attribute")
    coll.ensureIndex("title")
    // Add data
    coll += DBObject("arg1" -> "m1", "attribute" -> "title", "arg2" -> "Barack_Obama")
    coll += DBObject("arg1" -> "m2", "attribute" -> "title", "arg2" -> "United_States_of_America")
    coll += DBObject("arg1" -> "m3", "attribute" -> "title", "arg2" -> "Michelle_Obama")
    coll += DBObject("arg1" -> "m1", "attribute" -> "president_of", "arg2" -> "m2")
    coll += DBObject("arg1" -> "m1", "attribute" -> "husband_of", "arg2" -> "m3")
    coll
  }

  def collectEvents(coll: MongoCollection = collection, eventFile: String = "events.txt") {
    println("Querying...")
    val out = new FileWriter(eventFile)
    val startTime = System.currentTimeMillis()
    val dateQuery = MongoDBObject("attribute" -> "start_date")
    (coll find dateQuery).foreach { q =>
      val mid = q.get("arg1").toString()
      val name = getName(mid).getOrElse("None")
      out.write(mid + ":" + name + "\t")
      val attributes = attributesOf(mid)
      for (a <- attributes.keys) out.write(a + ":" + attributes(a) + ":" + getName(attributes(a)).getOrElse("None") + "\t")
      out.write("\n")
    }
    val time = (System.currentTimeMillis() - startTime) / 1000.0
    out.close()
    println("Event queries finished in %1.1fm".format(time/60))
  }

//  def findAllIn(s: Map[String, String]): Seq[String] = findAllIn(new MongoDBObject(s))
//
//  def findAllIn(q: MongoDBObject): Seq[String] = {
//
//  }

}

object FreebaseReader {
  // Loading Patterns
  val INSTANCE_PATTERN  = """<http://rdf.freebase.com/ns/([^>]+)>\t<http://rdf.freebase.com/ns/type.type.instance>\t<http://rdf.freebase.com/ns/([^>]+)>.*""".r
  val ATTRIBUTE_PATTERN = """<http://rdf.freebase.com/ns/(m.[^>]+)>\t<http://rdf.freebase.com/ns/([^>]+)>\t<http://rdf.freebase.com/ns/(m.[^>]+)>.*""".r
  val DATE_PATTERN      = """<http://rdf.freebase.com/ns/(m.[^>]+)>\t<http://rdf.freebase.com/ns/time.event.([^_]+_date)>\t\"(.+)\".*""".r
  val TITLE_PATTERN     = """<http://rdf.freebase.com/ns/(m.[^>]+)>\t<http://rdf.freebase.com/key/wikipedia.en>\t\"(.+)\".*""".r
  val TEXT_PATTERN      = """<http://rdf.freebase.com/ns/common.topic.description>\t\"(.+)\".*""".r

  // Interactive Patterns
  val METH_PATTERN_1 = """getCandidateMIDs\([^.*]\)""".r

  def main(args: Array[String]) = {
    if (args.length == 0) {
      println("Reading Freebase from existing KB...")
      val fb = loadFromDB()
      fb.test()
    }
    else if (args(0) == "--interactive") {
      println("Starting interactive Freebase console...")
      interactive()
    }
    else {
      println("Constructing new Freebase index from file <%s>...".format(args(0)))
      //val filename = "/Volumes/My Passport/freebase-rdf-latest.gz"
      assert(new File(args(0)).isFile, "File does not exist.")
      val fb = loadFromFile(args(0))
      fb.test()
      println("Finished.")
    }
  }

  def loadFromDB(port: Int = 27017): FreebaseReader = {
    println("Connecting to local Mongo database at port %d...".format(port))
    val mongoClient = MongoClient("localhost", port)
    val db = mongoClient("FB")
    println("Existing Collections:")
    println(db.collectionNames.map("\t" + _).mkString("\n"))
    new FreebaseReader(db("FB"))
  }

  def loadFromFile(filename: String, port: Int = 27017): FreebaseReader = {
    println("Loading triples...")
    val mongoClient = MongoClient("localhost", port)
    val db = mongoClient("FB")
    mongoClient.dropDatabase("FB")
    val coll = db("FB")
    val startTime = System.currentTimeMillis()
    var count = 0
    val reader = if (filename.endsWith(".gz")) new GZipReader(filename) else io.Source.fromFile(filename).getLines

    for (lines <- reader.grouped(1000000)) {
      val bulkBuilder = coll.initializeUnorderedBulkOperation
      for (line <- lines) {
        val cleaned = line.replaceAll("> +<", ">\t<").replaceAll("> +\"", ">\t\"")
        cleaned match {
          case INSTANCE_PATTERN(t, mid) => {
            bulkBuilder.insert(MongoDBObject("mid" -> mid, "type" -> t))
          }
          case ATTRIBUTE_PATTERN(mid1, attribute, mid2) => {
            bulkBuilder.insert(MongoDBObject("arg1" -> mid1, "attribute" -> attribute, "arg2" -> mid2))
          }
          case DATE_PATTERN(mid, dateType, date) => {
            bulkBuilder.insert(MongoDBObject("arg1" -> mid, "attribute" -> dateType, "arg2" -> date))
          }
          case TITLE_PATTERN(mid, title) => {
            bulkBuilder.insert(MongoDBObject("arg1" -> mid, "attribute" -> "title", "arg2" -> title))
          }
          case TEXT_PATTERN(mid, text) => {
            bulkBuilder.insert(MongoDBObject("arg1" -> mid, "attribute" -> "text", "arg2" -> text))
          }
          case _ =>
        }
        count += 1
      }
      print("Count: " + count / 1000000 + "M")
      try {
        val exec = bulkBuilder.execute
        println("...Inserted: " + exec.getInsertedCount)
      } catch {
        case e: Throwable => println("..." + e.getMessage)
      }
    }

    val time = (System.currentTimeMillis() - startTime) / 1000.0
    println("Finished loading Freebase triples (%d lines) in %1.1fm".format(count, time/60))
    // Specify indices
    coll.ensureIndex("mid")
    coll.ensureIndex("arg1")
    coll.ensureIndex("arg2")
    coll.ensureIndex("type")
    coll.ensureIndex("attribute")
    coll.ensureIndex("title")

    val itime = (System.currentTimeMillis() - startTime) / 1000.0
    println("Finished constructing indices in %1.1fm".format(itime / 60.0))

    println("There are %d mids.".format(coll.count("mid" $exists true)))
    println("There are %d rows with start dates.".format(coll.count(MongoDBObject("attribute" -> "start_date"))))
    new FreebaseReader(coll)
  }

  def interactive() = {
//    val scanner = new java.util.Scanner(System.in)
//    print("Enter MongoDB Query\n>")
//    val q = scanner.nextLine()
//    q match {
//      case
//    }
//    print("Executing query <%s>".format(q))
  }

}




















/*
    if (useExisting) {
      if (db.collectionExists("FB")) {
        println("Using existing indexes.")
        return db("FB")
      }
      else {
        println("No existing collection exists.")
      }
    }
    mongoClient.dropDatabase("FB")
    val coll = db("FB")
    coll
 */




/*
  def getName(mid: String): Option[String] = {
    collection findOne MongoDBObject("arg1" -> mid, "attribute" -> "title") match {
      case Some(record) => Some(record.get("arg2").toString)
      case _ => None
    }
  }

  def getDescription(mid: String): Option[String] = {
    collection findOne MongoDBObject("arg1" -> mid, "attribute" -> "text") match {
      case Some(record) => Some(record.get("arg2").toString)
      case _ => None
    }
  }

 */



//
//  def load(filename: String, port: Int = 27017, init: Boolean = true) = {
//    collection = mongoFromTriples(filename, port, init)
//  }


/*
      (coll find MongoDBObject("arg1" -> mid)).foreach { r =>
        if (r.contains("arg2") && r.contains("attribute")) {
          if (r.get("attribute") == "start_date" || r.get("attribute") == "end_date") {
            sb.append("\t" + r.get("arg1") + ":" + r.get("attribute") + ":" + r.get("arg2").toString())
          }
          else {
            sb.append("\t" + r.get("arg1") + ":" + r.get("attribute") + ":" + getName(r.get("arg2").toString(), coll).getOrElse("None"))
          }
        }
      }
 */





//    val filename = "/Users/narad/Downloads/freebase-1million.gz"
//        val filename = "/Users/narad/Downloads/events.gz"
//        val filename = "/Users/narad/Desktop/fb_test.gz"





//  def attributesOf(mid1: String, mid2: String, coll: MongoCollection): IndexedSeq[String] = {
//    (coll find MongoDBObject("arg1" -> mid1, "arg2" -> mid2)).flatMap { r =>
//      r("attribute") match {
//        case Some(attribute) => Some(attribute)
//        case _ => None
//      }
//    }.toIndexedSeq
//  }

//  val query1 = MongoDBObject("type" -> "event.disaster")




/*
An example of Casbah compound index:

db.collection.ensureIndex(MongoDBObject("Header.records.n" -> 1) ++ MongoDBObject("Header.records.v" -> 1) ++ MongoDBObject("Header.records.l" -> 1))

 */

/*
    var count = 0
     (coll find MongoDBObject("record" -> "yes")).foreach { q =>
      println(q)
      count += 1
    }
    println(count)
 */

//      val cols = line.replaceAll("> +<", ">\t<").replaceAll("\\.", "_").split("\t")
//      val record = MongoDBObject(cols(0) -> cols(1))
//      coll.insert(record)
//      println(count)

//    val query1 = MongoDBObject("<http://rdf.freebase.com/ns/american_football.football_player.footballdb_id>" -> "<http://www.w3.org/2000/01/rdf-schema#label>")
//    val query2 = MongoDBObject("foo" -> "bar", "type" -> "event")
//    val query3 = MongoDBObject("type" -> "event")
//    val query3 = MongoDBObject("foo")
//    coll.insert(query2)
//
//    println(coll findOne query1)
//    println(coll findOne query2)
//    println(coll findOne query3)







//    val db = new MongoDBObject
/*
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport

import collection.JavaConverters._

import org.json.simple.parser.JSONParser
import com.google.api.client.http._
import collection.mutable.{ArrayBuffer, HashSet}
import java.io.FileWriter

 */

  /*

  /** API Key for the registered developer project for your application. */
  private val API_KEY = "AIzaSyClJFx89pJR0_8yc1nvTClMUzFPj0r1dHA"

  /** Global instance of the HTTP transport. */
  private val httpTransport = GoogleNetHttpTransport.newTrustedTransport()

  /** Extraction Patterns **/
  private val MID_PATTERN = """\"mid\":\"([^\"]+)\"""".r
  private val PROP_PATTERN = """\"id\":\"([^\"]+)\"""".r

  val parser = new JSONParser()
  val requestFactory = httpTransport.createRequestFactory()

  val EVENT_PROPS = Array("start_date", "end_date", "locations", "people_involved", "includes_event", "instance_of_recurring_event", "comment")

  def properties(id: String): Array[String] = {
    val query = "[{\"id\":null,\"name\":null,\"type\":\"/type/property\",\"schema\":{\"id\":\"" + id + "\"}}]"
    val url = new GenericUrl("https://www.googleapis.com/freebase/v1/mqlread")
    url.put("query", query);
    url.put("key", API_KEY)
    val request = requestFactory.buildGetRequest(url)
    val httpResponse = request.execute()
    println("URL: " + url.toString())
    println("Request: " + request.toString())
    val response = parser.parse(httpResponse.parseAsString()).toString()
    (PROP_PATTERN findAllIn response).matchData.map { m =>
      val prop = m.group(1)
      prop.substring(prop.lastIndexOf("/")+1, prop.size)
    }.toArray.filter(_ != id)
  }

  def events() {
    val url = new GenericUrl("https://www.googleapis.com/freebase/v1/search")
 //   url.put("mid", "/m/0152ww")
    url.put("type", "/time/event")
    url.put("limit", "5")
    url.put("indent", "true")
    for (prop <- EVENT_PROPS) url.put(prop, null)
    url.put("key", API_KEY)

    val request = requestFactory.buildGetRequest(url)
    println("URL: " + url.toString())
    println("Request: " + request.toString())
    val httpResponse = request.execute()
    val response = parser.parse(httpResponse.parseAsString()).toString()
    (MID_PATTERN findAllIn response).matchData foreach { m =>
      println("group: " + m.group(1))
    }

    println(response.toString())
  }
}


object FreebaseReader {
  val COL_PATTERN = """\\<([^>]+)\\>.*""".r
  val MID_PATTERN = """http://rdf.freebase.com/ns/([^>]+)>""".r


  def main(args: Array[String]) = {
    mongoDB()

//    val filename = "/Users/narad/Desktop/fb_tiny.gz"

//    val filename = "/Users/narad/Downloads/freebase-rdf-2014-09-14-00-00.gz"
//    val mode = "ANDREAS"
//    if (mode == "ANDREAS") {
//      val regions = instances(filename, instance="location.statistical_region")
//      println("Statistical Regions:")
//      println(regions.mkString("\n"))
//      println("%d regions.\n".format(regions.size))
//      for (line <- new GZipReader(filename)) {
//        for (region <- regions) {
//          if (line.contains(region)) appendToFile(line, region + ".txt")
//        }
//      }
//    }
//    else {
//
//    }
//    val events = instances(filename, instance="event.disaster")
//    val props = properties(filename, events)
////    val regions = instances("/Users/narad/Downloads/freebase-rdf-2014-09-14-00-00.gz")
////    val out = new FileWriter("out.txt")
////    out.write(regions.mkString("\n"))
////    out.close()
  }

  def appendToFile(line: String, filename: String) = {
    val out = new FileWriter(filename, true)
    out.write(line + "\n")
    out.close()
  }

  def properties(fbFile: String, mids: Array[String]): Array[String] = {
    val ab = new ArrayBuffer[String]
    for (line <- new GZipReader(fbFile)) {
      val col2 = extractColumn(line, 2)
      if (col2 == "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>") {
//        println("--" + extractColumn(line, 3))
    //    ab += extractColumn(line, 3)
      }
    }
    ab.toArray
  }

  def instances(fbFile: String, instance: String = "location.statistical_region"): Array[String] = {
    val pattern = "<http://rdf.freebase.com/ns/%s>".format(instance)
    val ab = new ArrayBuffer[String]
    for (line <- new GZipReader(fbFile)) {
      if (line.startsWith(pattern)) {
        extractColumn(line, 3) match {
          case Some(col) => {
            extractMID(col) match {
              case Some(mid) => ab += mid
              case _=> {}
            }
          }
          case _=> {}
        }
      }
    }
    ab.toArray
  }

  def extractColumn(str: String, col: Int): Option[String] = {
    val cols = str.replaceAll(" +", " ").split(" |\t")
    if (cols.size >= col) Some(cols(col-1)) else None
  }

  def extractMID(str: String): Option[String] = {
    (MID_PATTERN findFirstMatchIn  str) match {
      case Some(mid) => Some(mid.group(1))
      case _ => None
    }
  }

  def mongoDB() = {
    val mongoColl = MongoClient()("casbah_test")("test_data")
    val db = MongoDBObject("foo" -> "bar",
                                   "x" -> "y",
                                "pie" -> 3.14,
                                "spam" -> "eggs")
    val q = MongoDBObject("foo" -> "bar")
    mongoColl.findOne(q).foreach { m =>
      println(m)
    }
    println("Done.")
  }

}

           */


//    val fr = new FreebaseReader
//    println(fr.properties("/time/event").mkString("\n"))
//    fr.events()
//
//    var events = new HashSet[String]
//    var lcount = 0
//    for (line <- new GZipReader(args(0))) {
//      val triples = line.split("\t")
////      assert(triples.size == 3, "Line contained irregular triple: \n%s".format(line))
////      println(line)
////      println(triples.size)
//      if (line.contains("event.disaster")) events += line
//      if (line.contains("0f8l9c")) println(line)
//      lcount += 1
//    }
//    println(events.size)
//    println("%d lines.".format(lcount))




//    val url = new GenericUrl("https://www.googleapis.com/freebase/v1/search")

//    val url = new GenericUrl("https://www.googleapis.com/freebase/v1/search")
//    "type": "/time/event"

//    url.put("filter", "(all type:/music/artist created:\"The Lady Killer\")")

//    url.put("query", "[{\"mid\": null,\"name\": null, \"type\": \"/location/statistical_region\",\"limit\": 100}]")
//    url.put("filter", "/location/statistical_region")
//    url.put("filter", "/time/event")


/*

    val url = new GenericUrl("https://www.googleapis.com/freebase/v1/search")
    url.put("query", "Cee Lo Green")
    url.put("filter", "(all type:/music/artist created:\"The Lady Killer\")")
    url.put("limit", "10")
    url.put("indent", "true")
    url.put("key", "AIzaSyClJFx89pJR0_8yc1nvTClMUzFPj0r1dHA")


 */


// initialize the transport
//   val httpTransport = GoogleNetHttpTransport.newTrustedTransport();

// set up global Freebase instance
/* client = new Freebase.Builder(httpTransport, JSON_FACTORY, null)
.setGoogleClientRequestInitializer(new FreebaseRequestInitializer(API_KEY))
.setApplicationName(APPLICATION_NAME).build();*/

// https://www.googleapis.com/freebase/v1/mqlread?query=[{%22/common/topic/article%22:[{%22id%22:null}],%22name%22:%22Ethanol%22,%22type%22:%22/medicine/drug%22}]&indent=1

//    val url = new GenericUrl("https://www.googleapis.com/freebase/v1/mqlread")
//    url.put("query", "bush")
//    url.put("filter", "(all type:/music/artist)")
//    url.put("limit", "100")

//   val url = new GenericUrl("https://www.googleapis.com/freebase/v1/topic")
// [{"mid": null,"name": null, "type": "/location/statistical_region","limit": 100}]'

//  url.put("query", "Bukit Panjang")
//  url.put("filter", "(any part_of:singapore)")
//    url.put("mid", null)
//    url.put("name", null)
//    url.put("filter", "/location/statistical_region")
//   url.put("type", "/location/statistical_region")
//    url.put("indent", "true")
//    url.put("html_escape","false")

//    url.put("query", "Bukit Panjang");
//    //url.put("filter", "(any type:/people/person domain:location/citytown/)");
//    url.put("filter", "(any part_of:singapore)");
//
//
//    url.put("limit", "10");
//    url.put("indent", "true");
//    url.put("key", API_KEY);
//    // System.out.println(url);

// System.out.println(response.size());
//   val results = response.get("result")



/*
class FreebaseReader {

  private val FREEBASE_API_VERSION = "v1"

  private val GOOGLE_BATCH_URL = "https://www.googleapis.com/batch"

  private val FREEBASE_SERVICE_URL = "https://www.googleapis.com/freebase/" + FREEBASE_API_VERSION

  private val BOUNDARY = "---theOpenRefineBoundary--="


  def events: Iterator[String] = {
    ???
  }

  def mqlread(query: String): String = {
    val url = new URL(GOOGLE_BATCH_URL)
    val service_url = FREEBASE_SERVICE_URL+"/mqlread";

    // We could use the javax.mail package, but it's actually more trouble than it's worth
    val body = "--" + BOUNDARY + "\n"
    + queryToMimeBodyPart("0", query, service_url, getApiKey())
    + "\n--" + BOUNDARY + "\n" ;

    HttpURLConnection connection = (HttpURLConnection) url.openConnection()
    connection.setRequestProperty("Content-Type","multipart/mixed; boundary="+ BOUNDARY);
    connection.setConnectTimeout(5000);
    connection.setDoOutput(true);

    Writer writer = new OutputStreamWriter(connection.getOutputStream());
    try {
      writer.write(body);
    } finally {
      writer.flush();
      writer.close();
    }

    connection.connect();
    String result = null;
    if (connection.getResponseCode() >= 400) {
      String responseMessage = connection.getResponseMessage();
      String errorStream = ParsingUtilities.inputStreamToString(connection.getErrorStream());
      LoggerFactory.getLogger("freebase").error(
        "Error in mqlreadMime: " + connection.getResponseCode() + ":" + responseMessage + " : "
        + errorStream);
    } else {
      InputStream is = connection.getInputStream();
      try {
        String s = ParsingUtilities.inputStreamToString(is);
        String boundary = s.substring(0,s.indexOf("\n"));
        boundary = boundary.split("\r")[0];
      String[] part = s.split(boundary); // part 0 is empty because of leading boundary
      String[] sections = part[1].split("\r\n\r\n");
      // Mime headers, followed by HTTP headers, followd by actual response
      result = sections[2];
      } finally {
        is.close();
      }
    }
    return result
  }

}


*/

