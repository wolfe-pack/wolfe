package ml.wolfe.nlp.io

import com.google.api.services.freebase

import java.net.URL;

/*
import com.freebase.api.Freebase;
import com.freebase.json.JSON;

import static com.freebase.json.JSON.o;
import static com.freebase.json.JSON.a;
*/

/**
 * Created by narad on 9/11/14.
 */

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport

import org.json.simple.parser.JSONParser
import com.google.api.client.http._



class FreebaseReader {

  /**
   * Be sure to specify the name of your application. If the application name is {@code null} or
   * blank, the application will log a warning. Suggested format is “MyCompany-ProductName/1.0″.
   */
  // private static final String APPLICATION_NAME = “DisambiguationUnitn”;

  /** API Key for the registered developer project for your application. */
  // private static final String API_KEY = “AIzaSyCpiRVznJZQcuYuOEJG2I3O7KmwuYjaD_Q”;

  /** Global instance of the JSON factory. */
  //private static final JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();

  //private static final GenericData properties = null;

  /** Global instance of the HTTP transport. */
  private val httpTransport = GoogleNetHttpTransport.newTrustedTransport()
  //val API_KEY = "


  def connect() {

    val parser = new JSONParser()
    val requestFactory = httpTransport.createRequestFactory()

    val url = new GenericUrl("https://www.googleapis.com/freebase/v1/search")
    url.put("query", "Cee Lo Green")
    url.put("filter", "(all type:/music/artist created:\"The Lady Killer\")")
    url.put("limit", "10")
    url.put("indent", "true")
    url.put("key", "AIzaSyClJFx89pJR0_8yc1nvTClMUzFPj0r1dHA")

    val request = requestFactory.buildGetRequest(url)
    val httpResponse = request.execute()
    val response = parser.parse(httpResponse.parseAsString())


    println(response.toString())
  }
}


object FreebaseReader {

  def main(args: Array[String]) = {
    val fr = new FreebaseReader
    fr.connect()
  }
}




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

