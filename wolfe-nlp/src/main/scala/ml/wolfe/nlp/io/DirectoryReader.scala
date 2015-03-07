package ml.wolfe.nlp.io

import java.io.File

import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 7/31/14.
 */
class DirectoryReader {

  def fileGroups(d: String): Map[String, Array[File]] = {
    fileGroups(new File(d))
  }

  def fileGroups(d: File): Map[String, Array[File]] = {
    d.listFiles.groupBy(f => removeExtension(f.getCanonicalPath))
  }

  def removeExtension(str: String): String = {
//    We should use the more extensible method from apache io-commons:
//    import org.apache.commons.io.FilenameUtils
//    d.listFiles.groupBy(f => FilenameUtils.removeExtension(f.getCanonicalPath))
//    But this is fine for now, without requiring an additional jar
    str.substring(0, str.lastIndexOf("."))
  }

  def listAllFiles(f: File): Array[File] = {
    if (f.isDirectory) {
      val files = new ArrayBuffer[File]()
      for (sf <- f.listFiles()) {
        files ++= listAllFiles(sf)
      }
      return files.toArray
    }
    else {
      return Array(f)
    }
  }

  def listAllFiles(f: File, pattern: String): Array[File] = {
    if (f.isDirectory) {
      val files = new ArrayBuffer[File]()
      for (sf <- f.listFiles()) {
        files ++= listAllFiles(sf, pattern)
      }
      return files.toArray
    }
    else {
      if (f.getName().matches(pattern)) {
        Array(f)
      }
      else {
        return Array()
      }
    }
  }
}
