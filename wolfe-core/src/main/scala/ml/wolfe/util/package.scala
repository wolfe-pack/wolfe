package ml.wolfe

import java.io.{FileOutputStream, File}
import java.nio.channels.Channels
import java.util.Calendar
import com.typesafe.config.{ConfigFactory, Config}
import scala.collection.mutable.ArrayBuffer

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
      val dir = new File(if (conf.hasPath("outDir")) conf.getString("outDir") else "out")
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

      for (resource <- addedResources) {
        val stream = getClass.getResourceAsStream("/" + resource)
        val in = Channels.newChannel(stream)
        val out = new FileOutputStream(new File(msDir,resource)).getChannel
        out.transferFrom(in,0l,Long.MaxValue)
        out.close()
        in.close()
      }

      createSymbolicLinkToLatest()
      msDir
    }
    val addedResources = new ArrayBuffer[String]
    var msDir: File = _
    var latest: File = _
    private var _conf: Config = ConfigFactory.parseFile(new File("conf/default.conf"))

    def createSymbolicLinkToLatest() =
      Runtime.getRuntime.exec("/bin/ln -sfn %s %s".format(msDir.getAbsolutePath, latest.getAbsolutePath))

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
}
