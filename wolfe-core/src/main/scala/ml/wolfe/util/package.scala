package ml.wolfe

import com.typesafe.config.Config

/**
 * Created by rockt on 26/10/2014.
 */
package object util {
  implicit class RichConfig(conf: Config) {
    import scala.collection.JavaConversions._

    def getStringList(path: String): List[String] =
      conf.getObjectList(path).asInstanceOf[java.util.ArrayList[String]].toList
  }
}
