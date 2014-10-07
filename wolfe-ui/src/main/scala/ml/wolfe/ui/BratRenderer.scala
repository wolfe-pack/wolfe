package ml.wolfe.ui

import java.io.File

import ml.wolfe.nlp.{Document, Sentence, Token}
import org.sameersingh.htmlgen.{HTML, RawHTML}

/**
 * @author Sebastian Riedel
 */
object BratRenderer {

  val bratInitScript = "/assets/initbrat.js"
  val bratLocation = "/assets/javascripts/brat"
  val headJS = bratLocation + "/client/lib/head.load.min.js"

  def wrapCode(id: String, collData: String, docData: String): HTML = {
    val webFontURLs =
      s"""
        |[
        |    '$bratLocation' + '/static/fonts/PT_Sans-Caption-Web-Regular.ttf',
        |    '$bratLocation' + '/static/fonts/Liberation_Sans-Regular.ttf'
        |]
      """.stripMargin

    val script =
      s"""
        |<script type="text/javascript">
        |        var bratLocation;
        |
        |             console.log("embedBrat");
        |             if (typeof bratLocation === 'undefined') {
        |                bratLocation = "/assets/javascripts/brat";
        |                head.js(
        |                    // External libraries
        |                    bratLocation + '/client/lib/jquery.min.js',
        |                    bratLocation + '/client/lib/jquery.svg.min.js',
        |                    bratLocation + '/client/lib/jquery.svgdom.min.js',
        |
        |                    // brat helper modules
        |                    bratLocation + '/client/src/configuration.js',
        |                    bratLocation + '/client/src/util.js',
        |                    bratLocation + '/client/src/annotation_log.js',
        |                    bratLocation + '/client/lib/webfont.js',
        |
        |                    // brat modules
        |                    bratLocation + '/client/src/dispatcher.js',
        |                    bratLocation + '/client/src/url_monitor.js',
        |                    bratLocation + '/client/src/visualizer.js'
        |                 );
        |                 console.log("head.js called");
        |             }
        |
        |             head.ready(function() {
        |                console.log("Head is ready");
        |
        |                var collData = $collData;
        |
        |                var docData = $docData;
        |
        |                Util.embed(
        |                    // id of the div element where brat should embed the visualisations
        |                    '$id',
        |                    // object containing collection data
        |                    collData,
        |                    // object containing document data
        |                    docData,
        |                    // Array containing locations of the visualisation fonts
        |                    $webFontURLs
        |                    );
        |            });
        |
        |
        |
        |    </script>
      """.stripMargin
    val html =
      s"""
        |
        | <div id="$id"></div>
        |
        | <link rel="stylesheet" type="text/css" href="/assets/javascripts/brat/style-vis.css"/>
        | <link rel="stylesheet" type="text/css" href="/assets/stylesheets/wolfe-brat.css"/>
        |
        |$script
      """.stripMargin
    println(html)
    RawHTML(html)
  }



  def bratTokens(doc:Document) = {
    val id = "brat" + Math.abs(doc.hashCode()).toString
    val collData =
      """
        |{
        |    entity_types: [ {
        |            type   : 'Token',
        |            /* The labels are used when displaying the annotion, in this case
        |                we also provide a short-hand "Per" for cases where
        |                abbreviations are preferable */
        |            labels : ['Token','Tok'],
        |            // Blue is a nice colour for a person?
        |            bgColor: '#fc0',
        |            // Use a slightly darker version of the bgColor for the border
        |            borderColor: 'darken'
        |    } ]
        |}
      """.stripMargin

    val tokenEntities = for ((t,i) <- doc.tokens.zipWithIndex) yield s"['T$i','Token',[[${t.offsets.start},${t.offsets.end}]]]"

    val sentenceBoundaries = for (s <- doc.sentences) yield s"[${s.offsets.start},${s.offsets.end}]"

    val tokenOffsets = for ((t,i) <- doc.tokens.zipWithIndex) yield s"[${t.offsets.start},${t.offsets.end}]"

    val docData =
      s"""
        |{
        |    // Our text of choice
        |    text     : "${doc.source}",
        |    // The entities entry holds all entity annotations
        |    entities : [
        |        /* Format: [{ID}, {TYPE}, [[{START}, {END}]]]
        |            note that range of the offsets are [{START},{END}) */
        |        ${tokenEntities.mkString(",\n")}
        |    ],
        |    sentence_offsets: [${sentenceBoundaries.mkString(",")}],
        |    token_offsets: [${tokenOffsets.mkString(",")}]
        |}
      """.stripMargin

    //Excluded this for now: |    '$bratLocation' + '/static/fonts/Astloch-Bold.ttf',

    wrapCode(id,collData,docData)

  }

  def main(args: Array[String]) {
    val token1 = Token("A",null)
    val token2 = Token("man",null)
    val sent = Sentence(Seq(token1,token2))
    val html = BratRenderer.bratTokens(Document("A man",Seq(sent)))
    val nb = new MutableMoroNotebook

    nb.html(html.source)

    val dir = new File("/Users/sriedel/projects/moro-notebooks/test")
    dir.mkdirs()
    nb.saveTo("Brat Notebook", new File(dir,"brat.json"))


  }

}
