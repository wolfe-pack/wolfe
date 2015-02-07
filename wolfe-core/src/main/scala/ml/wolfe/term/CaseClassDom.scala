package ml.wolfe.term


import ml.wolfe.term

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * @author riedel
 */
object CaseClassDom {

  //  def all[C,D1 <: Dom](dom1:D1):TypedDom[C] = macro allImpl[C,D1]
  //
  //  def allImpl[C, D1 <: Dom](c:whitebox.Context)(dom1:c.Expr[D1]):c.Expr[TypedDom[C]] = ???

  //http://imranrashid.com/posts/scala-reflection/

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    annottees.map(_.tree).toList match {
      case (param: ModuleDef) :: Nil =>
        val typeName = param.name.toTermName
//        println(c.prefix)
        val (valueType,argDoms) = c.prefix.tree match {
          case q"new ${_}[$typ](...${args})" => (typ,args)
          case _ => null
        }
        val targetTrait = c.prefix.tree match {
          case Apply(Select(New(AppliedTypeTree(Ident(_), List(typ))), termNames.CONSTRUCTOR), _) => typ
        }
        val tpe = c.typecheck(q"(7.asInstanceOf[$targetTrait])").tpe
        val typedArgDoms:List[Tree] = argDoms.head.map(a => c.typecheck(a))
//        println(typedArgDoms)
//        println(tpe)
//        println(tpe.members.filter(_.isConstructor))
//        println(argDoms)
        val constructor = tpe.members.filter(_.isConstructor).head
        val params = constructor.asMethod.paramLists.head

        val domArgs = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"val $name:$argDom.type = $argDom"
        }

        val termArgs = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"def $name:$argDom.Term"
        }
        val varArgsDef = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"def $name:$argDom.Var"
        }
        val constArgs = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"val $name:$argDom.Term = $argDom.const(__value.$name)"
        }
        val argNames = params.map(_.name.toTermName)
        val one = typedArgDoms.map(a => q"$a.one")
        val zero = typedArgDoms.map(a => q"$a.zero")

        def sum(offsets:List[Tree]):Tree = offsets match {
          case offset :: Nil => offset
          case head :: tail =>
            val s = sum(tail)
            q"$head + $s"
          case _ => ???
        }

        def concat(offsets:List[Tree]):Tree = offsets match {
          case offset :: Nil => offset
          case head :: tail =>
            val s = concat(tail)
            q"$head ++ $s"
          case _ => ???
        }


        val domNames = params.map(_.name.toTermName)
        val argLengths = domNames.map(a => q"dom.$a.lengths")

        def argOffset(initOffset:Tree, index:Int) = sum(initOffset :: argLengths.take(index))

        val anonArgNames = params.indices.map(i => TermName(c.freshName("arg"+i))).toIndexedSeq

        def offsetStatements(body:(TermName,TermName,Tree) => Tree) = for ((argDom,i) <- domNames.zipWithIndex) yield {
          val varName = anonArgNames(i)
          val offset = argOffset(q"offsets",i)
          body(varName,argDom,offset)
        }

        val argValues = offsetStatements {
          case (varName,argDom,offset) =>  q"val $varName:dom.$argDom.Value = dom.$argDom.toValue(setting,$offset)"
        }

        val marginalClassName = TypeName(tpe.typeSymbol.name.decodedName.toString + "Marginals")
        val marginalArguments = for (d <- domNames) yield {
          q"val $d:dom.$d.Marginals"
        }
        val marginalsCaseClass = q"""
          case class $marginalClassName(..$marginalArguments)
        """
        val argMarginals = for ((argDom,i) <- domNames.zipWithIndex) yield {
          val varName = anonArgNames(i)
          val offset = argOffset(q"offsets",i)
          q"val $varName:dom.$argDom.Marginals = dom.$argDom.toMarginals(msg,$offset)"
        }

        val copyMarginals = offsetStatements {
          case (_,argDom,offset) =>  q"dom.$argDom.copyMarginals(marginals.$argDom,msgs,$offset)"
        }

        val copyValues = offsetStatements {
          case (_,argDom,offset) =>  q"dom.$argDom.copyValue(value.$argDom,setting,$offset)"
        }

        //dom1.fillZeroMsgs(target, offsets)
        val fillZeroMsgs = offsetStatements {
          case (_,argDom,offset) =>  q"dom.$argDom.fillZeroMsgs(target,$offset)"
        }

        val lengths = sum(domNames.map(n => q"dom.$n.lengths"))
        val atoms = concat(domNames.map(n => q"domVar.$n.atoms"))


        val staticVarArgs = offsetStatements {
          case (_,argDom,offset) =>
            q"""val $argDom = dom.$argDom.variable(name + "." + "name",$offset, if (owner == null) this else owner)"""
        }

        val dynVarArgs = offsetStatements {
          case (_,argDom,offset) =>
            q"""val $argDom = dom.$argDom.dynamic(name + "." + "name",$offset, if (owner == null) this else owner)"""
        }





//        println(params.head.typeSignature)
//        val cast = valueType.asInstanceOf[Ident]
//        val typed = c.typecheck(valueType)
//        println(valueType.tpe)

        //println(valueType.)
//        val test = c.prefix
        val term = q"""
          object $typeName extends ml.wolfe.term.ProductDom {
            dom =>

            $marginalsCaseClass

            type Value = $valueType
            type Term = DomTerm
            type Var = DomVar
            type Marginals = $marginalClassName

            ..$domArgs

            def toValue(setting: Setting, offsets: Offsets) = {
              ..$argValues
              new Value(..$anonArgNames)
            }
            def toMarginals(msg: Msgs, offsets: Offsets) = {
              ..$argMarginals
              new $marginalClassName(..$anonArgNames)
            }
            def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
              ..$copyMarginals
            }
            def copyValue(value: Value, setting: Setting, offsets: Offsets) = {
              ..$copyValues
            }
            def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
              ..$fillZeroMsgs
            }
            def variable(name: String, statOffsets: Offsets, owner: ml.wolfe.term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
              val offsets = statOffsets
              ..$staticVarArgs
            }

            def dynamic(name: => String, dynOffsets: => Offsets, owner: ml.wolfe.term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
              def offsets = dynOffsets
              ..$dynVarArgs
            }
            def const(__value: Value) = new DomTermImpl {
              ..$constArgs
            }
            val lengths = $lengths
            def one = new $valueType(..$one)
            def zero = new $valueType(..$zero)

            trait DomTermImpl extends DomTerm with super.DomTermImpl {
              def arguments = IndexedSeq(..$argNames)
            }

            trait DomTerm extends super.DomTerm  {
              ..$termArgs
            }
            trait DomVar extends super.DomVar with DomTerm {
              domVar =>
              ..$varArgsDef
              def offsets: Offsets
              def atoms = $atoms
              def ranges = Ranges(offsets, offsets + dom.lengths)
            }

          }
        """
//        println(term)
        c.Expr[Any](term)
      case _ => c.abort(c.enclosingPosition,"Can't create domain")
    }
  }

  def implOnClass(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    annottees.map(_.tree).toList match {
      case (param: ModuleDef) :: Nil =>
        val typeName = param.name.toTermName
        //        println(c.prefix)
        val (valueType,argDoms) = c.prefix.tree match {
          case q"new ${_}[$typ](...${args})" => (typ,args)
          case _ => null
        }
        val targetTrait = c.prefix.tree match {
          case Apply(Select(New(AppliedTypeTree(Ident(_), List(typ))), termNames.CONSTRUCTOR), _) => typ
        }
        val tpe = c.typecheck(q"(7.asInstanceOf[$targetTrait])").tpe
        val typedArgDoms:List[Tree] = argDoms.head.map(a => c.typecheck(a))
        //        println(typedArgDoms)
        //        println(tpe)
        //        println(tpe.members.filter(_.isConstructor))
        //        println(argDoms)
        val constructor = tpe.members.filter(_.isConstructor).head
        val params = constructor.asMethod.paramLists.head

        val domArgs = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"val $name:$argDom.type = $argDom"
        }

        val termArgs = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"def $name:$argDom.Term"
        }
        val varArgsDef = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"def $name:$argDom.Var"
        }
        val constArgs = for ((param,argDom) <- params zip typedArgDoms) yield {
          val name = param.name.toTermName
          q"val $name:$argDom.Term = $argDom.const(__value.$name)"
        }
        val argNames = params.map(_.name.toTermName)
        val one = typedArgDoms.map(a => q"$a.one")
        val zero = typedArgDoms.map(a => q"$a.zero")

        def sum(offsets:List[Tree]):Tree = offsets match {
          case offset :: Nil => offset
          case head :: tail =>
            val s = sum(tail)
            q"$head + $s"
          case _ => ???
        }

        def concat(offsets:List[Tree]):Tree = offsets match {
          case offset :: Nil => offset
          case head :: tail =>
            val s = concat(tail)
            q"$head ++ $s"
          case _ => ???
        }


        val domNames = params.map(_.name.toTermName)
        val argLengths = domNames.map(a => q"dom.$a.lengths")

        def argOffset(initOffset:Tree, index:Int) = sum(initOffset :: argLengths.take(index))

        val anonArgNames = params.indices.map(i => TermName(c.freshName("arg"+i))).toIndexedSeq

        def offsetStatements(body:(TermName,TermName,Tree) => Tree) = for ((argDom,i) <- domNames.zipWithIndex) yield {
          val varName = anonArgNames(i)
          val offset = argOffset(q"offsets",i)
          body(varName,argDom,offset)
        }

        val argValues = offsetStatements {
          case (varName,argDom,offset) =>  q"val $varName:dom.$argDom.Value = dom.$argDom.toValue(setting,$offset)"
        }

        val marginalClassName = TypeName(tpe.typeSymbol.name.decodedName.toString + "Marginals")
        val marginalArguments = for (d <- domNames) yield {
          q"val $d:dom.$d.Marginals"
        }
        val marginalsCaseClass = q"""
          case class $marginalClassName(..$marginalArguments)
        """
        val argMarginals = for ((argDom,i) <- domNames.zipWithIndex) yield {
          val varName = anonArgNames(i)
          val offset = argOffset(q"offsets",i)
          q"val $varName:dom.$argDom.Marginals = dom.$argDom.toMarginals(msg,$offset)"
        }

        val copyMarginals = offsetStatements {
          case (_,argDom,offset) =>  q"dom.$argDom.copyMarginals(marginals.$argDom,msgs,$offset)"
        }

        val copyValues = offsetStatements {
          case (_,argDom,offset) =>  q"dom.$argDom.copyValue(value.$argDom,setting,$offset)"
        }

        //dom1.fillZeroMsgs(target, offsets)
        val fillZeroMsgs = offsetStatements {
          case (_,argDom,offset) =>  q"dom.$argDom.fillZeroMsgs(target,$offset)"
        }

        val lengths = sum(domNames.map(n => q"dom.$n.lengths"))
        val atoms = concat(domNames.map(n => q"domVar.$n.atoms"))


        val staticVarArgs = offsetStatements {
          case (_,argDom,offset) =>
            q"""val $argDom = dom.$argDom.variable(name + "." + "name",$offset, if (owner == null) this else owner)"""
        }

        val dynVarArgs = offsetStatements {
          case (_,argDom,offset) =>
            q"""val $argDom = dom.$argDom.dynamic(name + "." + "name",$offset, if (owner == null) this else owner)"""
        }





        //        println(params.head.typeSignature)
        //        val cast = valueType.asInstanceOf[Ident]
        //        val typed = c.typecheck(valueType)
        //        println(valueType.tpe)

        //println(valueType.)
        //        val test = c.prefix
        val term = q"""
          object $typeName extends ml.wolfe.term.ProductDom {
            dom =>

            $marginalsCaseClass

            type Value = $valueType
            type Term = DomTerm
            type Var = DomVar
            type Marginals = $marginalClassName

            ..$domArgs

            def toValue(setting: Setting, offsets: Offsets) = {
              ..$argValues
              new Value(..$anonArgNames)
            }
            def toMarginals(msg: Msgs, offsets: Offsets) = {
              ..$argMarginals
              new $marginalClassName(..$anonArgNames)
            }
            def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
              ..$copyMarginals
            }
            def copyValue(value: Value, setting: Setting, offsets: Offsets) = {
              ..$copyValues
            }
            def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
              ..$fillZeroMsgs
            }
            def variable(name: String, statOffsets: Offsets, owner: ml.wolfe.term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
              val offsets = statOffsets
              ..$staticVarArgs
            }

            def dynamic(name: => String, dynOffsets: => Offsets, owner: ml.wolfe.term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
              def offsets = dynOffsets
              ..$dynVarArgs
            }
            def const(__value: Value) = new DomTermImpl {
              ..$constArgs
            }
            val lengths = $lengths
            def one = new $valueType(..$one)
            def zero = new $valueType(..$zero)

            trait DomTermImpl extends DomTerm with super.DomTermImpl {
              def arguments = IndexedSeq(..$argNames)
            }

            trait DomTerm extends super.DomTerm  {
              ..$termArgs
            }
            trait DomVar extends super.DomVar with DomTerm {
              domVar =>
              ..$varArgsDef
              def offsets: Offsets
              def atoms = $atoms
              def ranges = Ranges(offsets, offsets + dom.lengths)
            }

          }
        """
        //        println(term)
        c.Expr[Any](term)
      case _ => c.abort(c.enclosingPosition,"Can't create domain")
    }
  }


}

class domain[T](doms: term.Dom*) extends StaticAnnotation {
  def macroTransform(annottees: Any*):Any = macro CaseClassDom.impl
}

