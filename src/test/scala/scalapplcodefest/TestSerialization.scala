package scalapplcodefest


import org.scalatest.{Matchers, WordSpec, FunSuite}
import scala.pickling._
import json._
import scala.pickling.binary.BinaryPickleFormat

/**
 * Created by larysa  26.02.14
 */
case class Person(i: Int)

class TestSerialization extends WordSpec with Matchers {
  val person = Person(2)
  val mapSer: Map[String, Boolean] = Map("x" -> true, "y" -> false)
  val group: Map[Person, String] = Map(Person(4) -> "M.", Person(6) -> "N.")

  "json serialization" should {
    val serialize = JsonSerializer.serialize(mapSer)

    val deseriMapi: Map[String, Boolean] = JsonSerializer.deserialize[Map[String, Boolean]](serialize)
    deseriMapi should be(mapSer)


    val seriPer = JsonSerializer.serialize(person)

    val deseriPer: Person = JsonSerializer.deserialize[Person](seriPer)
    deseriPer should be(person)

    JsonSerializer.serializeToFile(person, "/tmp/person.ser")
    val deserializedPerson: Person = JsonSerializer.deserializeFromFile[Person]("/tmp/person.ser")
    deserializedPerson should be(person)


    JsonSerializer.serializeToFile(group, "/tmp/group.ser")
    val deseriGroup: Map[Person, String] = JsonSerializer.deserializeFromFile[Map[Person, String]]("/tmp/group.ser")
    deseriGroup should be(group)
  }
  "binary serializer" should {
    val seriGroup = BinarySerializer.serialize(group)
    val deseri: Map[Person, String] = BinarySerializer.deserialize[Map[Person, String]](seriGroup.value)
    deseri should be(group)

    BinarySerializer.serializeToFile(group, "/tmp/groupBinary.ser")
    val deserGroup = BinarySerializer.deserializeFromFile[Map[Person, String]]("/tmp/groupBinary.ser")
    deserGroup should be(group)
    println("deserGroup = " + deserGroup)
  }


}
