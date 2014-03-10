package ml.wolfe.legacy.mln

import ml.wolfe._
import java.io.IOException
import scala.io.Source
import ml.wolfe.legacy.mln.MLNParser._
import scala.collection.mutable.ListBuffer
import scala.Symbol
import scala.collection.immutable.HashMap
import scala.Predef._
import ml.wolfe.legacy.value._
import ml.wolfe.legacy.term._
import ml.wolfe.legacy.TermDSL._
import ml.wolfe.legacy.mln.MLNParser.IntegerTypeDefinition
import ml.wolfe.legacy.mln.MLNParser.Atom
import ml.wolfe.legacy.mln.MLNParser.HardFormula
import ml.wolfe.legacy.value.RangeSet
import ml.wolfe.legacy.mln.MLNParser.AsteriskFormula
import ml.wolfe.legacy.mln.MLNParser.AsteriskAtom
import ml.wolfe.legacy.mln.MLNParser.WeightedFormula
import ml.wolfe.legacy.mln.MLNParser.And
import ml.wolfe.legacy.mln.MLNParser.DatabaseAtom
import ml.wolfe.legacy.mln.MLNParser.ConstantTypeDefinition
import ml.wolfe.legacy.term.Predicate
import ml.wolfe.legacy.mln.MLNParser.Equivalence
import ml.wolfe.legacy.mln.MLNParser.PlusVariable
import ml.wolfe.legacy.mln.MLNParser.Or
import ml.wolfe.legacy.term.LambdaAbstraction
import ml.wolfe.legacy.mln.MLNParser.PlusAtom
import ml.wolfe.legacy.term.Var
import ml.wolfe.legacy.mln.MLNParser.Not
import ml.wolfe.legacy.term.TupleTerm2
import ml.wolfe.legacy.term.CartesianProductTerm2
import ml.wolfe.legacy.mln.MLNParser.ExclamationVariable
import ml.wolfe.legacy.mln.MLNParser.VariableOrType
import ml.wolfe.legacy.term.FunApp
import ml.wolfe.legacy.mln.MLNParser.FunctionDefinition
import ml.wolfe.legacy.mln.MLNParser.Implies
import ml.wolfe.legacy._

/**
 * Created by larysa  05.12.13
 */
class MLNTranslator {

  import MLNParser._
  import TermDSL._


  private val typeDeclaration = new ListBuffer[Expression]
  private val predDeclarations = new ListBuffer[Expression]
  private val funDeclarations = new ListBuffer[Expression]
  private val mlnFormulae = new ListBuffer[Formula]
  private val groundAtoms = new ListBuffer[DatabaseAtom]
  private val dbFunctions = new ListBuffer[DatabaseFunction]

  private var domains = new HashMap[Symbol, ml.wolfe.legacy.term.Term[Any]]
  private var predicates = new HashMap[Symbol, Predicate[_, Boolean]]
  private var functions = new HashMap[Symbol, Predicate[_, _]]

  private var queryPredicate: Set[String] = Set()

  def getState: State = state(StateCollector(groundAtoms, predicates).toList: _*)
  def getPredicates = predicates
  def getFunctions = functions
  def getDomains = domains

  def getRawState: List[GroundAtom[_, Boolean]] = RawStateCollector(groundAtoms, predicates).rawState.toList

  def getPredicatesDictionary= {
    val predDictionary: ListBuffer[(Symbol, Seq[Term])] = predDeclarations map (p => {
      val atom: Atom = p.asInstanceOf[Atom]
      Symbol(atom.predicate)->atom.args
    })

    predDictionary.toMap
  }


  def mln(file: String): this.type = {
    val expr_parser = expression
    LoanResourceManager.withFileIteratorForMLN(file) {
      line => {
        val mln: ParseResult[Expression] = parse(expr_parser, line)
        processMLN(mln)
      }
    }
    this
  }

  def db(file: String): this.type = {
    val db_parser = MLNParser.db
    LoanResourceManager.withFileIteratorForMLN(file) {
      line => {
        val db: ParseResult[Any] = parse(db_parser, line)
        processDB(db)
      }
    }
    this
  }

  def query(pred: String*): this.type = {
    queryPredicate = pred.toSet
    this
  }

  def justTransform = {
    domains ++= typeDeclaration.map(t => TypeDeclaration(t).convert).toMap
    domains ++= DomainDeclaration(groundAtoms, predDeclarations).allDomains

    predicates ++= predDeclarations.map(p => PredicateDeclaration(p, domains).convert).toMap
    functions ++= funDeclarations.map(f => FunctionDeclaration(f, domains).convert).toMap
  }

  def getModel: FunApp[(FactorieVector, FactorieVector), Double] = {
    //    domains ++= typeDeclaration.map(t => TypeDeclaration(t).convert).toMap
    //    domains ++= DomainDeclaration(groundAtoms, predDeclarations).allDomains
    //
    //    predicates ++= predDeclarations.map(p => PredicateDeclaration(p, domains).convert).toMap
    //    functions ++= funDeclarations.map(f => FunctionDeclaration(f, domains).convert).toMap
    justTransform

    val folf = mlnFormulae.map(formula => FormulaDeclaration(formula, predicates).convert).flatten.toSeq

    val index = new Index()

    val features: Seq[Reduce[FactorieVector]] = folf map (formula => {
      val name: String = formula._1
      val function: ml.wolfe.legacy.term.Term[Boolean] = formula._3

      val unitVec = unit(index(Symbol(name)), I(function))
      val vars: Set[Var[Any]] = VarsCollector.collect(function.asInstanceOf[FunApp[Any, Boolean]])

      val lambdaFactory = LambdaAbstractionFactory(unitVec, vars)
      val featureVec = vectors.sum(lambdaFactory)
      featureVec
    })

    val weights = 'weights of vectors
    val mln: FunApp[(FactorieVector, FactorieVector), Double] = vectors.sumSeq(features) dot weights
    mln

    // the code below is just an example
    //    val feats: Seq[(Seq[Symbol], Double)] = folf map (f => (Seq(Symbol(f._1)), f._2))
    //    val concreteWeights = index.createDenseVector(feats.toList: _*)()
    //
    //    val condition = getState
    //
    //    val conditioned = mln | condition | weights -> concreteWeights
    //
    //    val queryPredicates: Set[Predicate[_, Boolean]] = queryPredicate map (q => predicates.get(Symbol(q)).get)
    //    val signature: Sig[_] = queryPredicates.toList match {
    //      case List(x) => throw new scala.Error("error: we do not provide sig for a one argument " + x)
    //      case List(x, y) => sig(x.asInstanceOf[Predicate[Any, Boolean]], y.asInstanceOf[Predicate[Any, Boolean]])
    //      case List(x, y, z) => sig(x.asInstanceOf[Predicate[Any, Boolean]], y.asInstanceOf[Predicate[Any, Boolean]], z.asInstanceOf[Predicate[Any, Boolean]])
    //    }
    //
    //    val lambda: LambdaAbstraction[_, Double] = lam(signature, conditioned)
    //    val maxi: RichMax[_] = max(lambda)
    //    val argmax = argState(maxi.byMessagePassing()).value()
    //
    //    println(argmax)

  }


  private def processMLN(mln_expr: ParseResult[Expression]): Unit = {
    mln_expr.get match {
      case IntegerTypeDefinition(_, _, _) | ConstantTypeDefinition(_, _) => typeDeclaration += mln_expr.get
      case Atom(_, _) => predDeclarations += mln_expr.get
      case FunctionDefinition(_, _, _) => funDeclarations += mln_expr.get
      case WeightedFormula(_, _) | HardFormula(_) | AsteriskFormula(_) |
           Implies(_, _) | Equivalence(_, _) | And(_, _) | Or(_, _) | Not(_) => mlnFormulae += mln_expr.get.asInstanceOf[Formula]

      case _ => println("unknown element = " + mln_expr.get.toString)
    }
  }

  private def processDB(db_atom: ParseResult[Any]): Unit = {
    db_atom.get match {
      case DatabaseAtom(_, _, _) => groundAtoms += db_atom.get.asInstanceOf[DatabaseAtom]
      case DatabaseFunction(_, _, _) => dbFunctions += db_atom.get.asInstanceOf[DatabaseFunction]
      case _ => println("unknown db_atom = " + db_atom.get)
    }
  }
}

trait MLN

trait TypeDeclaration extends MLN {
  def convert: (Symbol, ml.wolfe.legacy.term.Term[Set[Any]])
}

object TypeDeclaration {


  def apply(expression: Expression) = expression match {
    case expr@IntegerTypeDefinition(_, _, _) => new TypeDeclaration {
      def convert = (Symbol(expr.name), RangeSet(expr.from, expr.to).asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]])
    }
    case expr@ConstantTypeDefinition(_, _) => new TypeDeclaration {
      def convert = (Symbol(expr.name), set(expr.constants))
    }
  }

}

trait DomainDeclaration {
  def allDomains: Map[Symbol, ml.wolfe.legacy.term.Term[Any]]
}

object DomainDeclaration {

  import MLNParser.Constant

  def apply(groundAtoms: ListBuffer[DatabaseAtom], predicates: ListBuffer[Expression]) = new DomainDeclaration {
    def allDomains: Map[Symbol, ml.wolfe.legacy.term.Term[Any]] = {
      val allDomainsFromDb = for (p <- predicates; atom <- groundAtoms; if atom.predicate == p.asInstanceOf[Atom].predicate) yield {
        val args = p.asInstanceOf[Atom].args
        args match {
          case List(dom1) => Seq(dom1.toString -> atom.args.head.asInstanceOf[Constant].value)
          case List(dom1, dom2) => Seq(dom1.toString -> atom.args.head.asInstanceOf[Constant].value,
            dom2.toString -> atom.args.tail.head.asInstanceOf[Constant].value)
        }
      }
      // happy debugging ;)
      // group by the domain name, and then for each domain name collect domain objects as a set (deduplication)

      val doms = allDomainsFromDb.flatten.groupBy(_._1).map(x => x._1 -> x._2.map(y => y._2).toSet)

      // tearing my hair out!!! this:  SetTerm(d._2.toList:_*) took me 2h to figure out;
      // still have no idea why the casting here
      // def apply[T](elems:T*) = Constant(SetValue(elems:_*)) does not have any effect ?!
      val map: Map[Symbol, ml.wolfe.legacy.term.Term[Any]] = doms.map(d => (Symbol(d._1) -> set(d._2.toList: _*)))
      map
    }
  }

}

trait PredicateDeclaration extends MLN {
  def convert: (Symbol, Predicate[_, Boolean])
}

object PredicateDeclaration {
  def apply(expression: Expression, domains: Map[Symbol, ml.wolfe.legacy.term.Term[Any]]) = new PredicateDeclaration {
    def convert = {
      expression match {
        case expr@Atom(_, _) =>
          val args = expr.args.asInstanceOf[List[MLNParser.Variable]]
          val dom = deriveDomain(args, domains)
          val predicate = Symbol(expr.predicate)
          //todo: exclamation arguments: mutualy exclusive predicate
          dom match {
            case Seq(d1) => (predicate, Predicate(predicate, d1.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]], bools))
            case Seq(d1, d2) => (predicate, Predicate(predicate, c(d1.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]], d2.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]]), bools /*ml.wolfe.Constant(Bools)*/))
          }
      }
    }
  }

  private def deriveDomain(variable: List[MLNParser.Variable],
                           domains: Map[Symbol, ml.wolfe.legacy.term.Term[Any]]): Seq[ml.wolfe.legacy.term.Term[Any]] = {
    variable match {
      case List(a1) => Seq(domains.get(Symbol(a1.name)).get)
      case List(a1, a2) => Seq(domains.get(Symbol(a1.name)).get, domains.get(Symbol(a2.name)).get)
    }
  }
}

trait FunctionDeclaration extends MLN {
  def convert: (Symbol, Predicate[_, _])
}

object FunctionDeclaration {
  def apply(expression: Expression, domains: Map[Symbol, ml.wolfe.legacy.term.Term[Any]]) = new FunctionDeclaration {
    def convert: (Symbol, Predicate[_, _]) = expression match {
      case expr@FunctionDefinition(_, _, _) => {
        val returnType = expr.returnType
        val args = expr.types.asInstanceOf[List[MLNParser.Variable]]
        val inputDom = deriveDomain(args, domains)
        val outputDom = deriveDomain(List(returnType), domains)
        val funcName = Symbol(expr.name)
        inputDom match {
          case Seq(d1) => (funcName, Predicate(funcName, d1.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]], outputDom.head.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]]))
          case Seq(d1, d2) => (funcName, Predicate(funcName, c(d1.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]], d2.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]]), outputDom.head.asInstanceOf[ml.wolfe.legacy.term.Term[Set[Any]]]))
        }
      }
    }
  }

  private def deriveDomain(variable: List[MLNParser.Variable],
                           domains: Map[Symbol, ml.wolfe.legacy.term.Term[Any]]): Seq[ml.wolfe.legacy.term.Term[Any]] = {
    variable match {
      case List(a1) => Seq(domains.get(Symbol(a1.name)).get)
      case List(a1, a2) => Seq(domains.get(Symbol(a1.name)).get, domains.get(Symbol(a2.name)).get)
    }
  }
}

trait FormulaDeclaration extends MLN {
  def convert: Set[(String, Double, ml.wolfe.legacy.term.Term[Boolean])]
}

object FormulaDeclaration {
  def apply(expression: Expression, predicates: Map[Symbol, ml.wolfe.legacy.term.Predicate[_, Boolean]]) = new FormulaDeclaration {
    def convert: Set[(String, Double, ml.wolfe.legacy.term.Term[Boolean])] = {
      expression match {
        case exp@WeightedFormula(_, _) => Set((exp.formula.toString, exp.weight, formula(exp.formula, predicates)))
        case exp@HardFormula(_) => Set((exp.formula.toString, Double.PositiveInfinity, formula(exp.formula, predicates)))
        case exp@AsteriskFormula(_) => {
          val asterisks: List[(String, Double, ml.wolfe.legacy.term.Term[Boolean])] = for (f <- expandAsterisk(exp.formula)) yield (exp.formula.toString, 0.0, formula(f, predicates))
          asterisks.toSet
        }
        case Implies(_, _) | Equivalence(_, _) | And(_, _) | Or(_, _) | Not(_) => Set((expression.toString, 0.0, formula(expression.asInstanceOf[Formula], predicates)))
        case _ => throw new scala.Error("error: unknown expression " + expression)
      }

    }
  }


  private def formula(f: Formula, predicates: Map[Symbol, Predicate[_, Boolean]]): ml.wolfe.legacy.term.Term[Boolean] = {
    if (f.allPlusVariables.size != 0) {
      expandPlusVariable(f, predicates) foreach (expanded => formula(expanded, predicates))
    }

    f match {
      case Atom(predicate, args) => {
        val predDefinition: Predicate[_, Boolean] = predicates.get(Symbol(predicate)).get

        val funApp: FunApp[Any, Boolean] = args match {
          case List(a1) => {
            val variable: ml.wolfe.legacy.term.Term[Any] = a1 match {
              case VariableOrType(name) => Var(Symbol(name), predDefinition.funCandidateDom)
              case MLNParser.Constant(value) => ml.wolfe.legacy.term.Constant(value)
              case ExclamationVariable(name) => throw new UnsupportedOperationException("Exclamation variable processing is not supported for now..")
            }
            FunApp(predDefinition.asInstanceOf[FunTerm[Any, Boolean]], variable)
          }

          case List(a1, a2) => {
            val domains = predDefinition.funCandidateDom.asInstanceOf[CartesianProductTerm2[_, _]]
            val var1: ml.wolfe.legacy.term.Term[Any] = a1 match {

              case VariableOrType(name) => Var(Symbol(name), domains.a1)
              case MLNParser.Constant(value) => ml.wolfe.legacy.term.Constant(value)
              case ExclamationVariable(name) => throw new UnsupportedOperationException("Exclamation variable processing is not supported for now..")
            }
            val var2: ml.wolfe.legacy.term.Term[Any] = a2 match {
              case VariableOrType(name) => Var(Symbol(name), domains.a2)
              case MLNParser.Constant(value) => ml.wolfe.legacy.term.Constant(value)
              case ExclamationVariable(name) => throw new UnsupportedOperationException("Exclamation variable processing is not supported for now..")
            }
            FunApp(predDefinition.asInstanceOf[FunTerm[Any, Boolean]],
              TupleTerm2(var1.asInstanceOf[ml.wolfe.legacy.term.Variable[_]],
                var2.asInstanceOf[ml.wolfe.legacy.term.Variable[_]]))
          }

        }
        funApp
      }
      case Implies(lhs, rhs) => FunApp(bools.implies, TupleTerm2(formula(lhs, predicates), formula(rhs, predicates)))
      case Equivalence(lhs, rhs) => FunApp(bools.equiv, TupleTerm2(formula(lhs, predicates), formula(rhs, predicates)))
      case And(lhs, rhs) => FunApp(bools.and, TupleTerm2(formula(lhs, predicates), formula(rhs, predicates)))
      case Or(lhs, rhs) => FunApp(bools.or, TupleTerm2(formula(lhs, predicates), formula(rhs, predicates)))
      case Not(expr) => FunApp(bools.neg, formula(expr, predicates))
    }
  }

  /* When predicates in a formula are preceded by *, consider all possible ways in which * can be replaced by !
     * e.g *student(x) ^ *professor(x) is expanded into four formulas:
          student(x) ^ professor(x)
          !student(x) ^ professor(x)
          student(x) ^ !professor(x)
          !student(x) ^ !professor(x)    */
  private def expandAsterisk(formula: Formula): List[Formula] = {
    val expandedFormula = formula match {
      case AsteriskAtom(predicate, args) => Atom(predicate, args) :: MLNParser.Not(Atom(predicate, args)) :: Nil
      case Atom(predicate, args) => formula :: Nil
      case And(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield And(l, r)
      case Or(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield Or(l, r)
      case Implies(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield Implies(l, r)
      case Equivalence(lhs, rhs) =>
        for (l <- expandAsterisk(lhs); r <- expandAsterisk(rhs)) yield Equivalence(l, r)
      case MLNParser.Not(f) => throw new scala.Error("Negation is not allowed together with *-operator.")
      case _ => throw new scala.Error("Unknown operator." + formula)
    }
    expandedFormula
  }

  private def expandPlusVariable(formula: Formula, predicates: Map[Symbol, Predicate[_, Boolean]]): List[MLNParser.Formula] = {
    formula match {
      case PlusAtom(name, args) => {
        val predicateDef: Predicate[_, Boolean] = predicates.get(Symbol(name)).get
        args match {
          case List(a1) => {
            val domain = predicateDef.funCandidateDom.asInstanceOf[ml.wolfe.legacy.term.Constant[SetValue[_]]].value
            val instantiated = domain.map(v => Atom(name, List(MLNParser.Constant(v.toString))).asInstanceOf[Formula])
            instantiated.toList
          }
          case List(a1, a2) => {
            val domains = predicateDef.funCandidateDom.asInstanceOf[CartesianProductTerm2[_, _]]
            val firstArgument = a1 match {
              case PlusVariable(a) => {
                val dom1 = domains.a1.asInstanceOf[ml.wolfe.legacy.term.Constant[SetValue[_]]].value
                dom1.map(v => MLNParser.Constant(v.toString))
              }
              case _ => Seq(a1)
            }
            val secondArgument = a2 match {
              case PlusVariable(a) => {
                val dom2 = domains.a2.asInstanceOf[ml.wolfe.legacy.term.Constant[SetValue[_]]].value
                dom2.map(v => MLNParser.Constant(v.toString))
              }
              case _ => Seq(a2)
            }
            val reconstructed = for (first <- firstArgument; second <- secondArgument) yield Atom(name, List(first, second)).asInstanceOf[Formula]
            reconstructed.toList
          }
        }
      }
      case Atom(predicate, args) => formula :: Nil
      case And(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs, predicates); r <- expandPlusVariable(rhs, predicates)) yield And(l, r)
      case Or(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs, predicates); r <- expandPlusVariable(rhs, predicates)) yield Or(l, r)
      case Implies(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs, predicates); r <- expandPlusVariable(rhs, predicates)) yield Implies(l, r)
      case Equivalence(lhs, rhs) =>
        for (l <- expandPlusVariable(lhs, predicates); r <- expandPlusVariable(rhs, predicates)) yield Equivalence(l, r)
      case MLNParser.Not(f) => for (x <- expandPlusVariable(f, predicates)) yield MLNParser.Not(x)
      case _ => throw new scala.Error("Unknown operator.")
    }
  }
}


object VarsCollector {
  def collect(function: FunApp[Any, Boolean]): Set[Var[Any]] = {
    val variables: Set[Var[Any]] = function.function match {
      case p@Predicate(name, funCandidateDom, funRange) => function.arg match {
        case t@TupleTerm2(a1, a2) => Set(a1.asInstanceOf[Var[Any]], a2.asInstanceOf[Var[Any]])
        case _ => Set(function.arg.asInstanceOf[Var[Any]])
      }
      case _ => function.arg match {
        case t@TupleTerm2(a1, a2) => collect(a1.asInstanceOf[FunApp[Any, Boolean]]) ++ collect(a2.asInstanceOf[FunApp[Any, Boolean]])
      }
    }
    variables
  }
}

object LambdaAbstractionFactory {
  def apply(body: ml.wolfe.legacy.term.Term[FactorieVector], variables: Set[Var[Any]]): FunTerm[_, FactorieVector] = {
    variables.toList match {
      case x :: Nil => LambdaAbstraction(x, body)
      case List(x, y) => LambdaAbstraction(sig(VarSig(x), VarSig(y)), body)
      //      case head :: tail => LambdaAbstraction(head, apply(body, tail.toSet)).asInstanceOf[FunTerm[Any, _]]
    }
  }
}

trait RawStateCollector {
  def rawState: Set[GroundAtom[_, Boolean]]
}

object RawStateCollector {
  def apply(groundAtoms: ListBuffer[DatabaseAtom], predicates: Map[Symbol, Predicate[_, Boolean]]) = new RawStateCollector {
    def rawState: Set[GroundAtom[_, Boolean]] = {
      val grouped: Map[String, ListBuffer[DatabaseAtom]] = groundAtoms.groupBy(_.predicate)

      val groundedAtoms = predicates map (predicate => {
        val atoms: ListBuffer[DatabaseAtom] = grouped.get(predicate._1.name).get

        val predDefinition: Predicate[Any, Boolean] = predicate._2.asInstanceOf[Predicate[Any, Boolean]]
        predDefinition.funCandidateDom match {
          case s: ml.wolfe.legacy.term.Constant[_] => {
            atoms map (a => predDefinition.atom(Symbol(a.args(0).asInstanceOf[MLNParser.Constant].value)))
          }
          case c: CartesianProductTerm2[_, _] =>
            atoms map (a =>
              predDefinition.atom(
                Symbol(a.args(0).asInstanceOf[MLNParser.Constant].value),
                Symbol(a.args(1).asInstanceOf[MLNParser.Constant].value)))
          case _ => Set()
        }
      })
      groundedAtoms.flatten.toSet
    }
  }
}

object StateCollector {
  def apply(groundAtoms: ListBuffer[DatabaseAtom], predicates: Map[Symbol, Predicate[_, Boolean]]): Set[Assign[_]] = {
    val grouped: Map[String, ListBuffer[DatabaseAtom]] = groundAtoms.groupBy(_.predicate)

    val state = predicates map (predicate => {
      val atoms: ListBuffer[DatabaseAtom] = grouped.get(predicate._1.name).get
      val predDefinition = predicate._2.asInstanceOf[Predicate[Any, Boolean]]
      val assign = predDefinition.funCandidateDom match {
        case s: ml.wolfe.legacy.term.Constant[_] => {
          atoms map (a => Assign(predDefinition.atom(Symbol(a.args(0).asInstanceOf[MLNParser.Constant].value)), true))
        }
        case c: CartesianProductTerm2[_, _] =>
          atoms map (a => Assign(
            predDefinition.atom(
              Symbol(a.args(0).asInstanceOf[MLNParser.Constant].value),
              Symbol(a.args(1).asInstanceOf[MLNParser.Constant].value)),
            true))
        case _ => Set()
      }
      assign.toSet
    })

    state.flatten.toSet
  }
}

/* Util objects*/
object LoanResourceManager {
  /* File resource management, tailored for an (Alchemy) MLN file*/
  def withFileIteratorForMLN(file: String)(process: String => Unit) {
    val stream = util.Util.getStreamFromClassPathOrFile(file)
    try {
      val lines = Source.fromInputStream(stream).getLines().filter(nonMLNElements(_))
      lines foreach (line => process(line))
    } catch {
      case e: IOException => throw e
      case e: Exception => throw e
    } finally {
      if (stream != null) {
        try {
          stream.close()
        } catch {
          case e: Exception => throw e
        }
      }
    }
  }

  private def nonMLNElements(x: String): Boolean = {
    !((x startsWith "//") || (x isEmpty))
  }
}
