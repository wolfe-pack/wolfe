package ml.wolfe.nlp
import ml.wolfe.WolfeSpec


import scala.language.implicitConversions

/**
 * @author Ingolf Becker
 */
class ObjectGraphSpecs extends WolfeSpec {



  case class ParentChildRelation() extends ObjectGraphRelation {
    type Parent = testParent
    type Child = testChild
  }
  case class FictionalRelation() extends ObjectGraphRelation {
    type Parent = testParent
    type Child = testChild
  }
  case class testParent(name: String)
  case class testChild(name: String) {

    def getParent(implicit graph: ObjectGraph[ParentChildRelation]): testParent = graph.receive(this)

    def getFictionalParent(implicit graph: ObjectGraph[FictionalRelation]): testParent = graph.receive(this)
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
      implicit val graph = new SimpleObjectGraph[ParentChildRelation]
      val child = testChild("Test Child")
      val parent = testParent("Father")
      graph.link1to1(parent, child)
      child.getParent should be (parent)
    }

    "should choose the correct implicits" in {
      implicit val graph3 = new SimpleObjectGraph[ParentChildRelation]
      implicit val graph = new SimpleObjectGraph[FictionalRelation]
      implicit val graph2 = new SimpleObjectGraph[ParentChildRelation]
      val child = testChild("Test Child")
      val parent = testParent("Father")
      graph.link1to1(parent, child)
      child.getFictionalParent should be (parent)
    }

    "link one parent to all children" in {
      val graph = new SimpleObjectGraph[ParentChildRelation]
      val children = for (i <- 1 to 10) yield testChild(i.toString)
      val parent = testParent("Father")
      graph.link1toN( parent,children)
      children.forall(graph.receive(_) == parent) should be (true)
    }

    "distinguish different types of links" in {
      implicit val graph = new SimpleObjectGraph[ParentChildRelation]
      implicit val graph2 = new SimpleObjectGraph[FictionalRelation]
      val child = testChild("Test Child")
      val parent = testParent("Father")
      val parent2 = testParent("GodFather")
      graph.link1to1(parent, child)
      graph2.link1to1(parent2, child)
      child.getParent should not be child.getFictionalParent
    }

  }
}
