package scalapplcodefest.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.mutable
import scalapplcodefest._
import cc.factorie.WeightsSet
import cc.factorie.optimize.{Perceptron, OnlineTrainer, Trainer}
import scalapplcodefest.sbt.FactorieConverter

//import scalapplcodefest.Wolfe._

/**
 * @author Sebastian Riedel
 */
object OptimizedWolfe extends WolfeAPI {

  override def argmax[T](data: Iterable[T])
                        (where: (T) => Boolean)
                        (obj: (T) => Double) = macro implArgmax[T]

  override def argmin[T](dom: Iterable[T])
                        (where: (T) => Boolean)
                        (obj: (T) => Double) = macro implArgmin[T]

  def all[A, B](mapper: A => B)(implicit dom: Iterable[A]): Iterable[B] = macro implAll[A, B]

  def implAll[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(mapper: c.Expr[A => B])(dom: c.Expr[Iterable[A]]) = {
    import c.universe._
    DomainExpansions.register(c.Expr(c.macroApplication), mapper, dom)
    reify(dom.splice map mapper.splice)
  }

  def implArgmax[T: c.WeakTypeTag](c: Context)
                                  (data: c.Expr[Iterable[T]])
                                  (where: c.Expr[T => Boolean])
                                  (obj: c.Expr[T => Double]) = {

    import c.universe._

    val helper = new MacroHelper[c.type](c)
    import helper._

    //information from the enclosing context
    val metaData = MetaStructuredGraph(data.tree, where.tree, obj.tree)
    val graphName = newTermName(c.fresh("graph"))

    val result = q"""
      import scalapplcodefest._
      import scalapplcodefest.MPGraph._

      ${metaData.classDef}
      val $graphName = new ${metaData.graphClassName}(null)
      MaxProduct($graphName.${metaData.mpGraphName},5)
      $graphName.${metaData.structureName}.setToArgmax()
      $graphName.${metaData.structureName}.value()
    """

    c.Expr[T](result)

  }

  def implArgmin[T: c.WeakTypeTag](c: Context)
                                  (dom: c.Expr[Iterable[T]])
                                  (where: c.Expr[T => Boolean])
                                  (obj: c.Expr[T => Double]) = {

    import c.universe._

    val helper = new MacroHelper[c.type](c)
    import helper._

    //todo: check that domain dom is vector domain

    val gradientBased = MetaGradientBasedMinimizer(dom.tree, obj.tree)

//    println(gradientBased.trainingCode)
    gradientBased.trainingCode match {
      case Some(code) => c.Expr[T](context.resetLocalAttrs(code.tree))
      case _ => reify(BruteForceWolfe.argmin(dom.splice)(where.splice)(obj.splice))
    }
//    reify(BruteForceWolfe.argmin(dom.splice)(where.splice)(obj.splice))
  }

}


object DomainExpansions {

  import scala.reflect.internal.util.SourceFile

  case class DomainExpansion(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any])

  case class Location(start: Int, source: SourceFile)

  val byExpression = new mutable.HashMap[Context#Expr[Any], DomainExpansion]()
  val byPosition   = new mutable.HashMap[Location, DomainExpansion]()

  def register(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any]) = {
    val expansion = DomainExpansion(term, constructor, domain)
    byExpression(constructor) = expansion
    byPosition(Location(term.tree.pos.startOrPoint, term.tree.pos.source)) = expansion
  }

  def findByPosition(start: Int, source: SourceFile) = byPosition.get(Location(start, source))

}

class MacroHelper[C <: Context](val context: C) extends TransformHelper[C] with StructureHelper[C] with GradientBasedMinimizationHelper[C] {

  import context.universe._

  //mapping from val names to definitions (we use methods with no arguments as vals too)
  val vals = context.enclosingUnit.body.collect({
    case ValDef(_, name, _, rhs) => name -> rhs
    case DefDef(_, name, _, Nil, _, rhs) => name -> rhs
  }).toMap

  //mapping from val names to definitions (we use methods with no arguments as vals too)
  val valDefs = context.enclosingUnit.body.collect({
    case v: ValDef if !v.rhs.isEmpty => v.symbol -> v.rhs
    case d: DefDef if d.vparamss == Nil => d.symbol -> d.rhs
  }).toMap

  //mapping from val names to definitions (we use methods with no arguments as vals too)
  val noArgDefDefs = context.enclosingUnit.body.collect({
    case d: DefDef if d.vparamss == Nil => d.symbol -> d.rhs
  }).toMap

  //mapping from symbols to methods
  val defs = context.enclosingUnit.body.collect({
    case d: DefDef if d.vparamss != Nil => d.symbol -> d
  }).toMap

  //todo: should this be looking up by symbol?
  val classes = context.enclosingUnit.body.collect({
    case cd@ClassDef(_, name, _, _) => name -> cd
  }).toMap
  //todo: should this be looking up by symbol?
  //  val classes = context.enclosingRun.units.flatMap(_.body.collect({
  //    case cd@ClassDef(_, name, _, _) => name -> cd
  //  })).toMap


}


trait GradientBasedMinimizationHelper[C <: Context] {
  this: MacroHelper[C] =>

  import context.universe._

  case class MetaGradientBasedMinimizer(dom: Tree, obj: Tree) {

    //todo: do something with dom
    val instanceVariableName = newTermName("_instance")
    val indexVariableName    = newTermName("_index")

    val normalizedObj = replaceVals(betaReduce(replaceMethods(simplifyBlocks(obj), defs)), noArgDefDefs)

    //convert objective into sum if not already a sum
    val sum: Tree = normalizedObj match {
      case s@q"(${_}) => sum(${_})(${_})(${_})" => s
      case q"($weights) => $rhs" => q"($weights) => sum(Seq(0))(_ => true)($rhs)" //todo: add numeric
    }

    //now get per-instance objective and function argument
    val q"($weights) => sum($data)($pred)(($arg) => $perInstance)" = sum

    //replace instance variable in client code with instance variable that we use in factorie code
    val replaced: Tree = transform(perInstance, {
      case i: Ident if i.symbol == arg.symbol => Ident(instanceVariableName)
    })

    //val wolfe = rootMirror.getModuleByName(newTermName("scalapplcodefest.Wolfe"))

    //check annotation on original objective
    val trainer = simplifyBlocks(obj) match {
      case q"(${_}) => $rhs(${_})" =>
        println(rhs)
        val t: Tree = rhs
        t.symbol.annotations.collectFirst({
          //todo: use tpe symbol and check against MinByDescent symbol
          case Annotation(tpe, List(arg), _) if tpe.typeSymbol.name.encoded == "MinByDescent" => arg
        }) match {
          case Some(arg) => context.Expr[WeightsSet => Trainer](arg)
          case _ => reify((w: WeightsSet) => new OnlineTrainer(w, new Perceptron, 5))
        }
      case _ => reify((w: WeightsSet) => new OnlineTrainer(w, new Perceptron, 5))
    }

    //    val trainingCode = for (gradientCalculator <- None) yield {
    val trainingCode = for (gradientCalculator <- generateGradientCalculatorClass(replaced, weights, indexVariableName)) yield {
      val generator = context.Expr[GradientCalculator](context.resetAllAttrs(gradientCalculator))
      //      val trainerFor = reify((w: WeightsSet) => new OnlineTrainer(w, new Perceptron, 5))
      val trainingData = context.Expr[Iterable[Any]](data)
      val code = generateFactorieCode(generator, trainer, trainingData)
      code
    }

    //todo: we might want to replace _instance in the code with a fresh variable name


    //generate gradient calculator for arguments in the sum

    def generateFactorieCode[T](gradientCalculatorGenerator: Expr[GradientCalculator],
                                trainerFor: Expr[WeightsSet => Trainer],
                                trainingData: Expr[Iterable[T]], scale: Expr[Double] = context.literal(-1.0)) = reify({
      import cc.factorie.WeightsSet
      import cc.factorie.la.WeightsMapAccumulator
      import cc.factorie.util.DoubleAccumulator
      import cc.factorie.optimize._
      import scalapplcodefest.sbt._
      import scalapplcodefest._

      val _index = new Index
      val weightsSet = new WeightsSet
      val key = weightsSet.newWeights(new DenseVector(10000))
      val examples = for (_instance <- trainingData.splice) yield new Example {
        val gradientCalculator = gradientCalculatorGenerator.splice
        def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
          println("Instance: " + _instance)
          val weights = weightsSet(key).asInstanceOf[Vector]
          val (v, g) = gradientCalculator.valueAndGradient(weights)
          value.accumulate(v)
          gradient.accumulate(key, g, scale.splice)
        }
      }
      val trainer = trainerFor.splice(weightsSet)
      trainer.trainFromExamples(examples)
      FactorieConverter.toWolfeVector(weightsSet(key).asInstanceOf[Vector], _index)
    })

  }

  trait MetaGradientCalculator {
    def className: TypeName
    def construct: Tree
  }

  //todo: currently derivation happens on the untyped tree. This creates issues in the presence of implicits
  //now create a gradient calculator class
  def generateGradientCalculatorClass(term: Tree, weightVar: ValDef,
                                      indexName: TermName): Option[Tree] = {
    term match {
      case ApplyMinus(arg1, arg2) =>
        for (g1 <- generateGradientCalculatorClass(arg1, weightVar, indexName);
             g2 <- generateGradientCalculatorClass(arg2, weightVar, indexName)) yield
          q"""new GradientCalculator {
                val arg1 = $g1
                val arg2 = $g2
                def valueAndGradient(param: scalapplcodefest.Vector): (Double, scalapplcodefest.Vector) = {
                  val (v1,g1) = arg1.valueAndGradient(param)
                  val (v2,g2) = arg2.valueAndGradient(param)
                  println("g1:" + FactorieConverter.toWolfeVector(g1,$indexName))
                  println("g2:" + FactorieConverter.toWolfeVector(g2,$indexName))
                  (v1 - v2, g1 - g2)
                }
              }
          """

      case DotProduct(arg1, arg2) if !arg1.exists(_.symbol == weightVar.symbol) && arg2.symbol == weightVar.symbol =>
        val cleaned = arg1 // context.typeCheck(context.resetAllAttrs(arg1))
        Some( q"""new GradientCalculator {
          val coefficient = FactorieConverter.toFactorieSparseVector($cleaned,$indexName)
          def valueAndGradient(param: scalapplcodefest.Vector): (Double, scalapplcodefest.Vector) = {
            (coefficient dot param,coefficient)
        }}""")
      case q"max($sampleSpace)($pred)($obj)" =>
        //create MPGraph
        val typed = context.typeCheck(sampleSpace)
        val metaData = MetaStructuredGraph(typed, pred, obj, {
          case i: Ident => i.symbol == weightVar.symbol
          case _ => false
        })

        //need to replace occurences of weightVariable in objective with toWolfeVector(param)
        Some( q"""new GradientCalculator {
          ${metaData.classDef}
          val _graph = new ${metaData.graphClassName}($indexName)
          def valueAndGradient(param: scalapplcodefest.Vector): (Double, scalapplcodefest.Vector) = {
            _graph.${metaData.mpGraphName}.weights = param
            MaxProduct(_graph.${metaData.mpGraphName},5)
            (_graph.${metaData.mpGraphName}.value,_graph.${metaData.mpGraphName}.gradient)
        }}""")
      //        Some(q"???")
      case _ => None
    }
  }

}

trait GradientCalculator {
  def valueAndGradient(param: scalapplcodefest.Vector): (Double, scalapplcodefest.Vector)
}
