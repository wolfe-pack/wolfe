package scalapplcodefest.legacy.structure

import scala.collection.mutable
import scalapplcodefest.mln.MLNParser.Term
import scala._
import scalapplcodefest.legacy.term._

/**
 * Created by larysa  03.01.14
 */

trait StructLearner {
  def getGraph
}

object StructLearner {

  def apply(domains: Map[Symbol, scalapplcodefest.legacy.term.Term[Any]],
            predicates: Map[Symbol, Predicate[_, Boolean]],
            rawState: List[GroundAtom[_, Boolean]],
            predDict: Map[Symbol, Seq[Term]]) = new StructLearner {

    //    def allowed(first: (Symbol, Predicate[_, Boolean]), second: (Symbol, Predicate[_, Boolean])): Boolean = {
    //
    //      val firstDef = first._2.asInstanceOf[Predicate[Any, Boolean]]
    //      val secondDef = second._2.asInstanceOf[Predicate[Any, Boolean]]
    //
    //      val canBeCombined: Boolean = firstDef.funCandidateDom match {
    //        case s: Constant[_] => {
    //          if (first._1.name.equals(second._1.name)) false
    //          else {
    //            secondDef.funCandidateDom match {
    //              //todo: from here: equals  is just a hack! we need here named domains within the Predicate object
    //              case sd: Constant[_] => {
    //                printf("1x1 first = %s, second = %s \n", first._1, second._2)
    //                s.equals(sd)
    //              }
    //              case cd: CartesianProductTerm2[_, _] => {
    //                printf("1x2 first = %s, second = %s \n", first._1, second._2)
    //                s.equals(cd.a1) || s.equals(cd.a2)
    //              }
    //            }
    //          }
    //        }
    //        case c: CartesianProductTerm2[_, _] => {
    //          secondDef.funCandidateDom match {
    //            case sd: Constant[_] => {
    //              printf("2x1 first = %s, second = %s \n", first._1, second._2)
    //              sd.equals(c.a1) || sd.equals(c.a2)
    //            }
    //            case cd: CartesianProductTerm2[_, _] => {
    //              printf("2x2 first = %s, second = %s \n", first._1, second._2)
    //              if (first._1.name == second._1.name) true
    //              else c.a1.equals(cd.a1) || c.a1.equals(cd.a2) || c.a2.equals(cd.a1) || c.a2.equals(cd.a2)
    //            }
    //          }
    //        }
    //      }
    //      canBeCombined
    //    }

    def allowedEdge(first: Symbol, second: Symbol): Boolean = {

      val firstDomain = predDict.get(first).get
      val secondDomain = predDict.get(second).get

      var allowed = false

      firstDomain match {
        case Seq(a1) => {
          secondDomain match {
            case Seq(x1) if first == second => allowed = false // same unary predicates are not allowed to be connected.
            case Seq(x1, x2) if (a1 == x1 || a1 == x2) => allowed = true
            case Seq(x1) if a1 == x1 => allowed = true
          }
        }
        case Seq(a1, a2) => {
          secondDomain match {
            case Seq(x1) if (a1 == x1 || a2 == x1) => allowed = true
            case Seq(x1, x2) if (a1 == x1 || a2 == x1 || a1 == x2 || a2 == x2) => allowed = true
          }
        }
      }
      allowed
    }


    def getGraph: Unit = {

      // todo: create possible predicate links

      val potentialEdges: Seq[PotentialEdge] = (for (first <- predDict.keySet;
                                                     second <- predDict.keySet;
                                                     if allowedEdge(first, second)) yield PotentialEdge(first, second)).toSeq

      println(">> potentialEdges = " + potentialEdges)

      val potentialConnections = new mutable.HashSet[PotentialEdge]()

      potentialEdges foreach (p => {
        val reversed = PotentialEdge(p.n2, p.n1)
        if (!(potentialConnections.contains(p) || potentialConnections.contains(reversed))) potentialConnections.add(p)

      })

      println("uniqueElements = " + potentialConnections)

      // todo: create graph

      val database: Map[Symbol, List[GroundAtom[_, Boolean]]] = rawState.groupBy(_.predicate.name)


      potentialConnections map (pc => {
        val firstName: Symbol = pc.n1
        val secondNode: Symbol = pc.n2

        val firstGroundAtoms: List[GroundAtom[_, Boolean]] = database.get(firstName).getOrElse(List())
        val secondGroundAtoms: List[GroundAtom[_, Boolean]] = database.get(secondNode).getOrElse(List())

        val frequency: Double = measureOfFrequency(firstGroundAtoms, secondGroundAtoms)

        if (frequency > 0.0) {
          //todo: create an edge
          val firstArgs = predDict.get(firstName).get
          val firstNodes = Seq(PredicateNode(firstName, firstArgs), NegPredicateNode(firstName, firstArgs))

          val secondArgs = predDict.get(secondNode).get
          val secondNodes = Seq(PredicateNode(secondNode, secondArgs), NegPredicateNode(secondNode, secondArgs))

          val graph: Seq[Edge] = for (v1 <- firstNodes; v2 <- secondNodes) yield Edge(v1, v2, frequency)
          println("graph = " + graph)
        }
      })



      // todo: prune the edges
      // learn final mln


    }

    def measureOfFrequency(first: List[GroundAtom[_, Boolean]], second: List[GroundAtom[_, Boolean]]): Double = {
      var measure: Double = 0.0
      val total: Int = first.size * second.size
      if (total == 0) return measure

      var counter: Int = 0
      if (first.size >= 2 && second.size >= 2) {
        for (f <- first; s <- second) {
          f.arg match {
            case (f1, f2) => {
              s.arg match {
                case (s1, s2) => {
                  if (f.components.equals(s.components) && f1 == s1 && f1 == s2 && f2 == s1 && f2 == s2) {/* do nothing here, because we do not consider the same binary predicate*/}
                  else {
                    if (f1.equals(s1) || f1.equals(s2) || f2.equals(s1) || f2.equals(s2)) {
                      counter += 1
                    }
                  }
                }
                case s1: Symbol => {
                  if (f1 == s1 || f2 == s1) counter += 1
                }
              }
            }
            case f1: Symbol => {
              s.arg match {
                case (s1, s2) => {
                  if (f1 == s1 || f1 == s2) counter += 1
                }
                case s1: Symbol => {
                  if (f1 == s1) counter += 1
                }
              }
            }
          }
        }
      }
      measure = counter.toDouble / total.toDouble
      //      println("counter = " + counter + " total =" + total + " measure= " + measure)
      measure
    }
  }


  case class PotentialEdge(n1: Symbol, n2: Symbol)

  trait Node

  case class PredicateNode(name: Symbol, args: Seq[Term]) extends Node {
    override def toString: String = "Predicate: " + name + " with args: " + args
  }

  case class NegPredicateNode(name: Symbol, args: Seq[Term]) extends Node {
    override def toString: String = "Negated Predicate: " + name + " with args: " + args
  }

  case class Edge(n1: Node, n2: Node, weight: Double) {
    override def toString: String = n1.toString + "== " + weight + " ==" + n2.toString
  }

  def learn(): Unit = {

  }

}

