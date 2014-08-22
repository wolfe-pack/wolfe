package ml.wolfe.ui

import java.io.{PrintStream, File}

import eu.henkelmann.actuarius.ActuariusTransformer

/**
 * @author Sebastian Riedel
 */
trait NotebookRenderer {

  val transformer = new ActuariusTransformer

  def html(text:String)

  def md(markdown:String): Unit = {
    html(transformer(markdown))
  }
  def h1(text:String) = html(s"<h1>$text</h1>")
  def h2(text:String) = html(s"<h1>$text</h1>")
  def source(src:String) = html(s"<pre>$src</pre>")

  def close()

}

class HTMLFileRenderer(dst:File, template: String => String = SimpleTemplate) extends NotebookRenderer {

  dst.getParentFile.mkdirs()

  val tmp = File.createTempFile(dst.getName,"html")
  val builder = new StringBuilder
  val out = new PrintStream(tmp)

  def html(text: String) = builder append text
  def close() = {
    out.println(template(builder.toString()))
    out.close()
    tmp.renameTo(dst)
  }
}

object SimpleTemplate extends (String => String) {

  def apply(content: String) = {
    s"""
        |<!DOCTYPE html>
        |<html lang="en">
        |
        |<head>
        |    <script type="text/javascript">
        |        var last = ""
        |        setInterval(function () {
        |                var href = document.URL;
        |                var xmlhttp = new XMLHttpRequest();
        |                xmlhttp.open("GET", href, false);
        |                xmlhttp.send();
        |                var changed = last != "" && last != xmlhttp.responseText;
        |                //console.log("Changed: " + changed);
        |                if (changed) {
        |                    window.location = href;
        |                }
        |                last = xmlhttp.responseText;
        |            },
        |            100);
        |    </script>
        |</head>
        |
        |<body>
        |$content
        |</body>
        </html>
    """.stripMargin
  }
}
