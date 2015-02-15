package ml.wolfe.term


import ml.wolfe.term

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * @author riedel
 */
object CaseClassDom {


  //http://imranrashid.com/posts/scala-reflection/


  def implOnClass(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    annottees.map(_.tree).toList match {
      case (caseClassDef: ClassDef) :: _ =>

        //case class World(rains:Boolean, prob:Double)

        //Token
        val caseClassTermName = caseClassDef.name.toTermName
        val caseClassTypeName = caseClassDef.name.toTypeName

        //TokenDom
        val domClassName = TypeName(c.freshName(caseClassTermName.decodedName.toString + "Dom"))

        //List(val rains:...,val prob:...)
        val q"case class ${_}(..${caseClassArgs: List[ValDef]})" = caseClassDef
        val caseClassArgTypeNames = caseClassArgs.map{case q"${_} val ${_}:$typ = ${_}" => typ}
        //println(caseClassArgTypes)
//        val typedCaseClassArgs = caseClassArgs.map(a => c.typecheck(a))
//        val typedCaseClassArgs = caseClassArgs.map(a => q"{${c.typecheck(a)}}").map{case q"{$a}" => a}
//        println(caseClassArgs.map(a => q"{c.typecheck(a)}"))

        //List(rains,prob)
        val caseClassArgNames = caseClassArgs.map(_.name)
        def argNames = caseClassArgNames
        val argName2Index = argNames.zipWithIndex.toMap

        //List(Boolean,Double)
//        println(caseClassArgTypeNames.map(a => a.symbol.info))
        //val caseClassArgTypeNames = typedCaseClassArgs.map(a => a.symbol.info)

        //List(Dom_rains, Dom_prob)
        val arg2DomTypeName = argNames.map(n => n -> TypeName(c.freshName("Dom_") + n.toString)).toMap
        val argDomTypeNames = argNames.map(arg2DomTypeName)

        //List(Dom_rains <: TypedDom[Boolean],...)
        val argDomTypeParameters = for ((argDomTypeName, caseClassArgTypeName) <- argDomTypeNames zip caseClassArgTypeNames) yield {
          q"type $argDomTypeName <: ml.wolfe.term.TypedDom[$caseClassArgTypeName]"
        }

        //List(val rains:Dom_rains,...)
        val domConstructorArgs = for ((arg, typ) <- argNames zip argDomTypeNames) yield q"val $arg:$typ"

        //List(rains.type,prob.type)
        val argDomSingletonTypes = argNames.map(n => tq"$n.type")

        //for named self reference
        val self = q"_dom" //TermName(c.freshName("dom"))
        val selfDef = q"val $self:$domClassName[..$argDomSingletonTypes]"

        //for Marginals class
        val marginalArguments = for (d <- argNames) yield {
          q"val $d:$self.$d.Marginals"
        }

        val marginalsCaseClassDef = q"""
             case class Marginals(..$marginalArguments)
          """

        //lengths of argument domains
        val argLengths = argNames.map(a => q"$self.$a.lengths")

        //provides the offsets for each argument
        def argOffsets(initOffset: Tree, argName: TermName) = reduce(initOffset :: argLengths.take(argName2Index(argName)))

        //to combine a list of trees into one
        def reduce(arguments: List[Tree],
                   op: (Tree, Tree) => Tree = (arg1: Tree, arg2: Tree) => q"$arg1 + $arg2"): Tree = arguments match {
          case offset :: Nil => offset
          case head :: tail =>
            val s = reduce(tail, op)
            op(head, s)
          case _ => ???
        }

        //the length of this domain
        val lengths = reduce(argLengths)

        //create a list of trees with a given argument name (_1) and offsets (_2)
        def withOffsets(initOffsets: Tree = q"_offsets")(body: (TermName, Tree) => Tree) = {
          for (argName <- argNames) yield {
            val offsets = argOffsets(initOffsets, argName)
            body(argName, offsets)
          }
        }

        val toValueArgs = withOffsets(q"_offsets") { case (name, off) => q"val $name = $self.$name.toValue(_setting, $off)"}
        val toMarginalsArgs = withOffsets(q"_offsets") { case (name, off) => q"val $name = $self.$name.toMarginals(_msgs, $off)"}
        val copyValueStatements = withOffsets() {case (name,off) => q"$self.$name.copyValue(_value.$name, _setting, $off)"}
        val copyMarginalStatements = withOffsets() {case (name,off) => q"$self.$name.copyMarginals(_marginals.$name, _msgs, $off)"}
        val fillZeroMsgsStatements = withOffsets() {case (name,off) => q"$self.$name.fillZeroMsgs(_target, $off)"}
        val ones = argNames.map(n => q"$n.one")
        val zeros = argNames.map(n => q"$n.zero")

        val termArgsDef = argNames.map(n => q"def $n:${arg2DomTypeName(n)}#Term")
        val varArgsDef = argNames.map(n => q"def $n:${arg2DomTypeName(n)}#Var")

        val atoms = reduce(argNames.map(n => q"_domVar.$n.atoms"),{case (a1,a2) => q"$a1 ++ $a2"})

        val staticVarArgs = withOffsets(q"_offsets") {
          case (name, off) =>
            q"""def $name = $self.$name.variable(name + "." + "name",$off, if (owner == null) this else owner)"""
        }

        val dynVarArgs = withOffsets(q"_offsets") {
          case (name, off) =>
            q"""def $name = $self.$name.dynamic(name + "." + "name",$off, if (owner == null) this else owner)"""
        }

        val constArgs = withOffsets(q"_offsets") {
          case (name, off) =>
          q"val $name:$self.$name.Term = $self.$name.const(_value.$name)"
        }

        val composedArgs = argNames.map(n => q"_term.$n.asInstanceOf[ml.wolfe.term.Term[ml.wolfe.term.Dom]]")


        val newTerm = q"""
          $caseClassDef
          class $domClassName[..$argDomTypeParameters](..$domConstructorArgs) extends ml.wolfe.term.ProductDom {
            _dom =>

            type Value = $caseClassTypeName
            type Var = DomVar
            type Term = DomTerm

            def lengths = $lengths

            def one = new $caseClassTypeName(..$ones)
            def zero = new $caseClassTypeName(..$zeros)

            $marginalsCaseClassDef

            def toValue(_setting:Setting,_offsets:Offsets):Value = {
              ..$toValueArgs
              new $caseClassTypeName(..$argNames)
            }

            def toMarginals(_msgs:Msgs,_offsets:Offsets):Marginals = {
              ..$toMarginalsArgs
              new Marginals(..$argNames)
            }

            def copyValue(_value:Value,_setting:Setting,_offsets:Offsets) {
              ..$copyValueStatements
            }

            def copyMarginals(_marginals: Marginals, _msgs: Msgs, _offsets: Offsets) = {
              ..$copyMarginalStatements
            }


            def fillZeroMsgs(_target: Msgs, _offsets: Offsets) = {
              ..$fillZeroMsgsStatements
            }

            def variable(name: String, _offsets: Offsets, owner: ml.wolfe.term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
              val offsets = _offsets
              ..$staticVarArgs
            }

            def dynamic(name: => String, _offsets: => Offsets, owner: ml.wolfe.term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
              def offsets = _offsets
              ..$dynVarArgs
            }

            def const(_value: Value) = new DomTermImpl {
              ..$constArgs
            }

            trait DomTermImpl extends DomTerm with super.DomTermImpl { _term =>
              type ArgumentType = ml.wolfe.term.Term[Dom]
              def arguments:IndexedSeq[ml.wolfe.term.Term[Dom]] = IndexedSeq(..$composedArgs)
              def copy(args:IndexedSeq[ArgumentType]) = ???
            }

            trait DomTerm extends super.DomTerm  {
              ..$termArgsDef
            }

            trait DomVar extends super.DomVar with DomTerm {
              _domVar =>
              ..$varArgsDef
              def offsets: Offsets
              def atoms = $atoms
              def ranges = Ranges(offsets, offsets + $self.lengths)
            }

          }
          object $caseClassTermName {
            def Dom[..$argDomTypeParameters](implicit ..$domConstructorArgs) =
              new $domClassName[..$argDomSingletonTypes](..$argNames)
          }
        """
        //def dom:$domClassName = new $domClassName
//        println(newTerm)

        c.Expr[Any](newTerm)
      case _ => c.abort(c.enclosingPosition, "Can't create domain")
    }
  }

}

class domain extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CaseClassDom.implOnClass
}

class identity extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro identityImpl.impl

}

object identityImpl {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = annottees.map(_.tree).toList
    c.Expr[Any](q"..$result")
  }

}
