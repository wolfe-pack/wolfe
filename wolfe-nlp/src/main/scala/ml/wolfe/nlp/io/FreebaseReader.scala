package ml.wolfe.nlp.io

import com.mongodb.casbah.Imports._
import scala.collection.mutable.HashMap

/**
 * Created by narad on 9/11/14.
 */

class FreebaseReader {
  val INSTANCE_PATTERN = """<http://rdf.freebase.com/ns/([^>]+)>\t<http://rdf.freebase.com/ns/type.type.instance>\t<http://rdf.freebase.com/ns/([^>]+)>.*""".r
  val RELATION_PATTERN = """<http://rdf.freebase.com/ns/(m.[^>]+)>\t<http://rdf.freebase.com/ns/([^>]+)>\t<http://rdf.freebase.com/ns/(m.[^>]+)>.*""".r
  val DATE_PATTERN     = """<http://rdf.freebase.com/ns/(m.[^>]+)>\t<http://rdf.freebase.com/ns/time.event.([^_]+_date)>\t\"(.+)\".*""".r
  val TITLE_PATTERN    = """<http://rdf.freebase.com/ns/(m.[^>]+)>\t<http://rdf.freebase.com/key/wikipedia.en>\t\"(.+)\".*""".r
  val midToTitle = new HashMap[String, String]
  var collection = null.asInstanceOf[MongoCollection]

  def mongoFromTriples(filename: String, port: Int = 27017, init: Boolean = true): MongoCollection = {
    println("Connecting to local Mongo database at port %d...".format(port))
    val mongoClient = MongoClient("localhost", port)
    if (init) mongoClient.dropDatabase("FB")
    val db = mongoClient("FB")
    println("Collections:")
    println(db.collectionNames.map("\t" + _).mkString("\n"))
    val coll = db("FB")

    println("Constructing Freebase indices...")
    var count = 0
    for (line <- new GZipReader(filename)) {
      val cleaned = line.replaceAll("> +<", ">\t<").replaceAll("> +\"", ">\t\"")
      cleaned match {
        case INSTANCE_PATTERN(itype, mid) => {
          coll.insert(MongoDBObject("type" -> itype, "mid" -> mid))
        }
        case RELATION_PATTERN(mid1, relation, mid2) => {
          coll.insert(MongoDBObject("mid1" -> mid1, "relation" -> relation, "mid2" -> mid2))
        }
        case DATE_PATTERN(mid, dateType, date) => {
          coll.insert(MongoDBObject("mid1" -> mid, "relation" -> dateType, "mid2" -> date))
        }
        case TITLE_PATTERN(mid, title) => {
          midToTitle(mid) = title
          coll.insert(MongoDBObject("mid1" -> mid, "title" -> title, "named" -> "yes"))
        }
        case _ =>
      }
      count += 1
    }
    println("Finished loading Freebase triples.")
    coll
  }

  def getName(mid: String, coll: MongoCollection): Option[String] = {
    return midToTitle.get(mid) // Much faster to keep a separate hash here, especially in low memory scenarios
    coll findOne MongoDBObject("mid1" -> mid, "named" -> "yes") match {
      case Some(record) => Some(record.get("title").toString())
      case _ => None
    }
  }

  def eventQueries(coll: MongoCollection) {
    println("Querying...")
    val query1 = MongoDBObject("type" -> "event.disaster")
    (coll find query1).foreach { q =>
      val sb = new StringBuilder
      val mid = q.get("mid").toString()
      val name = getName(mid, coll).getOrElse("None")
      sb.append(mid + ":" + name)
      (coll find MongoDBObject("mid1" -> mid)).foreach { r =>
        if (r.contains("mid2") && r.contains("relation")) {
          sb.append("\t" + r.get("mid2") + ":" + r.get("relation") + ":" + getName(r.get("mid2").toString(), coll).getOrElse("None"))
        }
      }
      println(sb.toString)
    }
  }

  def load(filename: String) = collection = mongoFromTriples(filename)

  def printEvents = eventQueries(collection)
}

object FreebaseReader {
  def main(args: Array[String]) = {
    val filename = args(0)
    //    val filename = "/Users/narad/Downloads/freebase-1million.gz"
    //    val filename = "/Users/narad/Downloads/events.gz"
//        val filename = "/Users/narad/Desktop/fb_test.gz"
    val fb = new FreebaseReader
    fb.load(filename)
    fb.printEvents
    println("Done.")
  }
}







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

