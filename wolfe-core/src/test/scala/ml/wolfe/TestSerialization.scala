//package ml.wolfe
//
///**
// * Created by larysa  27.02.14
// */
//
//
//import scala.pickling._
//import json._
//import scala.pickling.binary.BinaryPickleFormat
//import org.scalatest.{FlatSpec, Matchers}
//import scala.language.existentials
//
///**
// * Created by larysa  26.02.14
// */
//class TestSerialization extends FlatSpec with Matchers {
//
//  case class Person(i: Int)
//
//  val mapSer: Map[String, Boolean] = Map("x" -> true, "y" -> false)
//  val person                       = Person(2)
//  val group : Map[Person, String]  = Map(Person(4) -> "M.", Person(6) -> "N.")
//
//
////  "json serializer" should "serialize case class" in {
////    val seriPer = JsonSerializer.serialize(person)
////    val deseriPer: Person = JsonSerializer.deserialize[Person](seriPer)
////    deseriPer should be(Person(2))
////  }
//  "json serializer" should "serialize map" in {
//    val serialize = JsonSerializer.serialize(mapSer)
//    val deseriMapi: Map[String, Boolean] = JsonSerializer.deserialize[Map[String, Boolean]](serialize)
//    deseriMapi should be(mapSer)
//  }
////  "json serializer" should "serialize case class to file" in {
////    JsonSerializer.serializeToFile(person, "/tmp/person.ser")
////    val deserializedPerson: Person = JsonSerializer.deserializeFromFile[Person]("/tmp/person.ser")
////    deserializedPerson should be(person)
////  }
////  "json serializer" should "serialize map to file" in {
////    JsonSerializer.serializeToFile(group, "/tmp/group.ser")
////    val deseriGroup: Map[Person, String] = JsonSerializer.deserializeFromFile[Map[Person, String]]("/tmp/group.ser")
////    deseriGroup should be(group)
////  }
//  "binary serializer" should "serialize map" in {
//    val seriGroup = BinarySerializer.serialize(group)
//    val deseri: Map[Person, String] = BinarySerializer.deserialize[Map[Person, String]](seriGroup.value)
//    deseri should be(group)
//  }
//
//  "binary serializer" should "serialize map to file" in {
//    BinarySerializer.serializeToFile(group, "/tmp/groupBinary.ser")
//    val deserGroup = BinarySerializer.deserializeFromFile[Map[Person, String]]("/tmp/groupBinary.ser")
//    deserGroup should be(group)
//  }
//  "binary serializer" should "serialize case class" in {
//    val seriPer = BinarySerializer.serialize(person)
//    val deseriPer: Person = BinarySerializer.deserialize[Person](seriPer.value)
//    deseriPer should be(Person(2))
//  }
//
//}
//
