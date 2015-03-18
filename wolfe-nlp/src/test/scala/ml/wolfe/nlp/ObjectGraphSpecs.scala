package ml.wolfe.nlp
import ml.wolfe.{nlp, WolfeSpec}

/**
 * @author Ingolf Becker
 */
class ObjectGraphSpecs extends WolfeSpec {

  case class ParentChildRelation() extends ObjectGraphRelation
  case class FicitonalRelation() extends ObjectGraphRelation
  case class testParent(name: String)
  case class testChild(name: String)

  "A simple object graph" should {

    "link one parent to one child" in {
      val graph = new SimpleObjectGraph[testParent, testChild]
      val child = testChild("Test Child")
      val relation = ParentChildRelation()
      val parent = testParent("Father")
      graph.link1to1(relation, parent, child)
      graph.receive(relation,child) should be (parent)
    }

    "link one parent to all children" in {
      val graph = new SimpleObjectGraph[testParent, testChild]
      val children = for (i <- 1 to 10) yield testChild(i.toString)
      val relation = ParentChildRelation()
      val parent = testParent("Father")
      graph.link1toN(relation, parent,children)
      children.forall(graph.receive(relation,_) == parent) should be (true)
    }

    "distinguish different types of links" in {
      val graph = new SimpleObjectGraph[testParent, testChild]
      val child = testChild("Test Child")
      val relation = ParentChildRelation()
      val relation2 = FicitonalRelation()
      val parent = testParent("Father")
      val parent2 = testParent("GodFather")
      graph.link1to1(relation, parent, child)
      graph.link1to1(relation2, parent2, child)
      println(graph.receive(relation, child))
      println(graph.receive(relation2, child))
      graph.receive(relation, child) should not be graph.receive(relation2, child)
    }

  }

}
