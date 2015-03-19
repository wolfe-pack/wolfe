package ml.wolfe.nlp
import ml.wolfe.{nlp, WolfeSpec}


import scala.language.implicitConversions

/**
 * @author Ingolf Becker
 */
class ObjectGraphSpecs extends WolfeSpec {



  case class ParentChildRelation() extends ObjectGraphRelation {
    type Parent = testParent
    type Child = testChild
    case class FatherSonRelation() extends RelationType
  }
  case class FicitonalRelation() extends ObjectGraphRelation {
    type Parent = testParent
    type Child = testChild
    case class DifferentRelation() extends RelationType
  }
  case class testParent(name: String)
  case class testChild(name: String) {

    def getParent(implicit graph: ObjectGraph[ParentChildRelation]): testParent = graph.receive(this)

    def getFictionalParent(implicit graph: ObjectGraph[FicitonalRelation]): testParent = graph.receive(this)
  }

  "A simple object graph" should {

    "link one parent to one child" in {
      val graph = new SimpleObjectGraph[ParentChildRelation]
      val child = testChild("Test Child")
      val parent = testParent("Father")
      graph.link1to1(parent, child)
      graph.receive(child) should be (parent)
    }

    "allow implicit access to parent" in {
      val graph = new SimpleObjectGraph[ParentChildRelation]
      val child = testChild("Test Child")
      val parent = testParent("Father")
      graph.link1to1(parent, child)
      println(graph)
      child.getParent(graph) should be (parent)
    }
//
//    "should choose the correct implicits" in {
//      val graph3 = new SimpleObjectGraph[ParentChildRelation]
//      val graph = new SimpleObjectGraph[FicitonalRelation]
//      val graph2 = new SimpleObjectGraph[ParentChildRelation]
//      val child = testChild("Test Child")
//      val parent = testParent("Father")
//      graph.link1to1(parent, child)
//      child.getFictionalParent should be (parent)
//    }
//
//    "link one parent to all children" in {
//      val graph = new SimpleObjectGraph[ParentChildRelation]
//      val children = for (i <- 1 to 10) yield testChild(i.toString)
//      val parent = testParent("Father")
//      graph.link1toN( parent,children)
//      children.forall(graph.receive(_) == parent) should be (true)
//    }
//
//
//
//    "distinguish different types of links" in {
//      val graph = new SimpleObjectGraph[ParentChildRelation]
//      val graph2 = new SimpleObjectGraph[ParentChildRelation]
//      val child = testChild("Test Child")
//      val parent = testParent("Father")
//      val parent2 = testParent("GodFather")
//      graph.link1to1(parent, child)
//      graph2.link1to1(parent2, child)
//      println(child.getParent)
//      println(child.getFictionalParent)
//      child.getParent should not be child.getFictionalParent
//    }

  }

}
