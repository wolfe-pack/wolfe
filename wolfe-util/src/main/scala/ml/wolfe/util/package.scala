package ml.wolfe

import java.io.{IOException, File, FileWriter}
import java.nio.file.{Paths, Path, Files}
import java.util.Calendar

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * @author sriedel
 * @author rockt
 */
package object util {
  implicit class RichConfig(conf: Config) {
    import scala.collection.JavaConversions._

    def getStringList(path: String): List[String] =
      conf.getObjectList(path).asInstanceOf[java.util.ArrayList[String]].toList
  }

  object Conf {
    lazy val targetSlots = conf.getString("target-slots").split(",").map(_.trim).toSet
    lazy val parentOutDir = {
      val dir = new File(if (conf.hasPath("outDir")) conf.getString("outDir") else "data/out")
      dir.mkdirs()
      dir
    }
    lazy val outDir = {
      val date = Calendar.getInstance()
      val day = date.get(Calendar.DAY_OF_MONTH)
      val month = date.get(Calendar.MONTH) + 1
      val year = date.get(Calendar.YEAR)
      val hour = date.get(Calendar.HOUR_OF_DAY)
      val min = date.get(Calendar.MINUTE)
      val sec = date.get(Calendar.SECOND)
      val ms = date.get(Calendar.MILLISECOND)

      val dayDir = new File(parentOutDir, "%d_%d_%d".format(day, month, year))
      msDir = new File(dayDir, "run_%d_%d_%d_%d".format(hour, min, sec, ms))

      latest = new File(parentOutDir, "latest")
      if (latest.exists()) {
        latest.delete()
      }
      msDir.mkdirs()

      //write config files to output directory
      for (resource <- addedResources) {
        val lines = Source.fromFile(resource).getLines()
        val fileWriter = new FileWriter(msDir.getAbsolutePath + "/" + resource.split("/").last)
        fileWriter.write(lines.mkString("\n"))
        fileWriter.close()
      }

      createSymbolicLinkToLatest()
      msDir
    }
    val addedResources = new ArrayBuffer[String]
    var msDir: File = _
    var latest: File = _
    private var _conf: Config = ConfigFactory.parseFile(new File("conf/default.conf"))

    def createSymbolicLinkToLatest() =
      try {
        Files.createLink(Paths.get(latest.getAbsolutePath), Paths.get(msDir.getAbsolutePath))
      } catch {
        case e: IOException => println("Can't create link")
      }
      //Runtime.getRuntime.exec("/bin/ln -sfn %s %s".format(msDir.getAbsolutePath, latest.getAbsolutePath))

    import scala.collection.JavaConversions._

    def add(filePath: String) {
      addedResources += filePath
      //_conf = ConfigFactory.parseResources(resources).withFallback(conf)
      _conf = ConfigFactory.parseFile(new File(filePath)).withFallback(conf)
      _conf.resolve()
    }

    def conf = _conf

    def getString(path: String): String = conf.getString(path)
    def getStringList(path: String): List[String] = conf.getStringList(path).asInstanceOf[java.util.ArrayList[String]].toList
    def getBoolean(path: String): Boolean = conf.getBoolean(path)
    def getInt(path: String): Int = conf.getInt(path)
    def getDouble(path: String): Double = conf.getDouble(path)
    def hasPath(path: String): Boolean = conf.hasPath(path)
  }

  object OverrideConfig {
    def apply(overrideWith: Map[String, Any], pathToNewConf: String = "conf/mf-overridden.conf", pathToOldConf: String = "conf/mf.conf"): String = {
      val conf = ConfigFactory.parseFile(new File(pathToOldConf)).entrySet().toString //string representations
                 .replaceAll("\"\\[|(Simple)?Config[^(]*\\(|\\]\"", "") //remove Config.*(
                 .split("\\),").mkString("\n").drop(1).dropRight(2).split("\n").toList.map(_.trim) //clean up
                 .map(s => s.split("=").head -> s.split("=").tail.mkString("=")) //map to key/value tuples
                 .toMap

      //println(ConfigFactory.parseFile(new File(pathToOldConf)).entrySet().toString)

      val overridden = conf ++ overrideWith.map { case (key, value) => value match {
        case s: String => key -> ("\"" + s + "\"")
        case seq: Seq[_] => key -> ("[" + seq.map(a => {
            if (a.isInstanceOf[String]) "\"" + a + "\"" else a
          }).mkString(", ") + "]")
        case _ => key -> value
      }}.toMap

      //println(overridden.mkString("\n"))

      val fileWriter = new FileWriter(pathToNewConf)
      val newConfMap = overridden.toList.map { case (key, value) =>
        val keys = key.split("\\.").toList
        if (keys.size > 1) (keys.head, keys.tail.mkString(".") + " = " + value)
        else ("", keys.mkString(".") + " = " + value)
      }.groupBy(_._1).mapValues(_.map(_._2))

      newConfMap.keys.foreach(k => {
        if (k.isEmpty) fileWriter.write(newConfMap(k).mkString("\n") + "\n")
        else fileWriter.write(k + " {\n" + newConfMap(k).mkString("\t", "\n\t", "\n") + "}\n")
      })

      fileWriter.close()
      pathToNewConf
    }
  }

  def getTimeString(seconds: Int): String = {
    def buildTimeString(seconds: Int, acc: String): String = {
      if (seconds < 60) acc + "%ds".format(seconds)
      else if (seconds < 3600) acc + buildTimeString(seconds % 60, "%dm ".format(seconds / 60))
      else if (seconds < 86400) acc + buildTimeString(seconds % 3600, "%dh ".format(seconds / 3600))
      else if (seconds < 604800) acc + buildTimeString(seconds % 86400, "%dd ".format(seconds / 86400))
      else if (seconds < 4233600) acc + buildTimeString(seconds % 604800, "%dw ".format(seconds / 604800))
      else "very long"
    }
    buildTimeString(seconds, "")
  }

  def getTimeString(milliseconds: Long): String = getTimeString((milliseconds / 1000).toInt)
}
