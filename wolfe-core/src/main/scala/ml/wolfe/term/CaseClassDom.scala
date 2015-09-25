package ml.wolfe.term

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

        //println(caseClassDef.name)

        //TokenDom
        val domClassName = TypeName(c.freshName(caseClassTermName.decodedName.toString + "Dom"))

        //List(val rains:...,val prob:...)
        val q"case class ${_}[..${genTypes: List[TypeDef]}](..${caseClassArgs: List[ValDef]})" = caseClassDef

        val caseClassArgTypeNames: List[Tree] = caseClassArgs.map { case q"${_} val ${_}:$typ = ${_}" => typ }

        //List(rains,prob)
        val caseClassArgNames = caseClassArgs.map(_.name)
        def argNames = caseClassArgNames
        def prefixName(prefix: String, name: TermName) = TermName(prefix + name.decodedName.toString)

        val argName2Index = argNames.zipWithIndex.toMap

        //List(Dom_rains, Dom_prob)
        val arg2DomTypeName = argNames.map(n => n -> TypeName(c.freshName("Dom_") + n.toString)).toMap
        val argDomTypeNames = argNames.map(arg2DomTypeName)

        //        val covariantArgDomTypeParameters = for (t <- argDomTypeParameters) yield q"+$t"
        def covariant(typeExpr: TypeDef): TypeDef = {
          TypeDef(Modifiers(typeExpr.mods.flags | Flag.COVARIANT), typeExpr.name, typeExpr.tparams, typeExpr.rhs)
        }
        //          val cc = q"case class A[+$typeExpr](t:Any)"
        //          val q"case class A[$hack](..${_})" = cc
        //          hack
        //        }

        //List(Dom_rains <: TypedDom[Boolean],...)
        val argDomTypeParameters: List[TypeDef] = {
          for ((argDomTypeName, caseClassArgTypeName) <- argDomTypeNames zip caseClassArgTypeNames)
            yield {
              q"type $argDomTypeName <: ml.wolfe.term.TypedDom[$caseClassArgTypeName]"
            }
        }

        val covariantArgDomTypeParameters = argDomTypeParameters map (t => covariant(t))

        //map (t => covariant(t))

        val genTypenames = for (t <- genTypes) yield tq"${t.name}"

        def createRightTypeHack(typeName: TypeName, bound: Tree) = {
          val q"object Blah { type Test2[$t] = Double }" = q"object Blah { type Test2[$typeName <: ml.wolfe.term.TypedDom[$bound]] = Double}"
          t
        }
        val argDomTypeParametersHack: List[TypeDef] = for ((argDomTypeName, caseClassArgTypeName) <- argDomTypeNames zip caseClassArgTypeNames)
          yield createRightTypeHack(argDomTypeName, caseClassArgTypeName)

        val typeParameterHack = genTypes ::: argDomTypeParametersHack
        val covariantMergedTypeParameters = genTypes ::: covariantArgDomTypeParameters
        val mergedTypeParameters = genTypes ::: argDomTypeParameters
        val mergedTypeParametersNames = genTypenames ::: argDomTypeParameters.map(t => tq"${t.name}")

        val argDomTypeLowerBounds = for (caseClassArgTypeName <- caseClassArgTypeNames) yield tq"ml.wolfe.term.TypedDom[$caseClassArgTypeName]"
        val mergedTypeBounds = genTypenames ::: argDomTypeLowerBounds

        //List(val rains:Dom_rains,...)
        val domConstructorArgs = for ((arg, typ) <- argNames zip argDomTypeNames) yield q"val $arg:$typ"

        //List(rains.type,prob.type)
        val argDomSingletonTypes = argNames.map(n => tq"$n.type")
        val mergedSingletonTypes = genTypenames ::: argDomSingletonTypes
        val mergedTypesFromTypeParameters = genTypenames ::: argDomTypeNames.map(n => tq"$n")

        //println(mergedSingletonTypes)

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
        val argDimensions = argNames.map(a => q"$self.$a.dimensions")

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
        val dimensions = reduce(argDimensions)


        //create a list of trees with a given argument name (_1) and offsets (_2)
        def withOffsets(initOffsets: Tree = q"_offsets")(body: (TermName, Tree) => Tree) = {
          for (argName <- argNames) yield {
            val offsets = argOffsets(initOffsets, argName)
            body(argName, offsets)
          }
        }

        val toValueArgs = withOffsets(q"_offsets") { case (name, off) => q"val $name = $self.$name.toValue(_setting, $off)" }
        val toMarginalsArgs = withOffsets(q"_offsets") { case (name, off) => q"val $name = $self.$name.toMarginals(_Msg, $off)" }
        val copyValueStatements = withOffsets() { case (name, off) => q"$self.$name.copyValue(_value.$name, _setting, $off)" }
        val copyMarginalStatements = withOffsets() { case (name, off) => q"$self.$name.copyMarginals(_marginals.$name, _Msg, $off)" }
        val fillZeroMsgStatements = withOffsets() { case (name, off) => q"$self.$name.fillZeroMsg(_target, $off)" }
        val ones = argNames.map(n => q"$n.one")
        val zeros = argNames.map(n => q"$n.zero")
        val sparseZeros = argNames.map(n => q"$n.sparseZero")

        val termArgsDef = argNames.map(n => q"def $n:${arg2DomTypeName(n)}#Term")
        val varArgsDef = argNames.map(n => q"def $n:${arg2DomTypeName(n)}#Term")

        val termConstructorArgs = argNames.map(n => q"val ${prefixName("_", n)}:$self.$n.Term")
        val termConstructorDefs = argNames.map(n => q"val $n:$self.$n.Term = ${prefixName("_", n)}")

        def termCastedArgs(args: Tree) = argNames.zipWithIndex.map {
          case (n, i) =>
            val indexConst = Constant(i)
            q"$args($indexConst).asInstanceOf[$self.$n.Term]"
        }
        val castArgs = termCastedArgs(q"args")

        val staticVarArgs = withOffsets(q"ml.wolfe.term.Offsets.zero") {
          case (name, off) =>
            val nameString = name.decodedName.toString
            val nameConst = Constant(nameString)
            q"""val $name:$self.$name.Term = $self.$name.own(new Field(this,$self.$name,$off)($nameConst))"""
          //
          //            q"""def $name = $self.$name.variable(name + "." + $nameConst,$off, if (owner == null) this else owner)"""
        }

        val constArgs = withOffsets(q"_offsets") {
          case (name, off) =>
            q"val $name:$self.$name.Term = $self.$name.Const(_value.$name)"
        }

        val composedArgs = argNames.map(n => q"_term.$n.asInstanceOf[ml.wolfe.term.Term[ml.wolfe.term.Dom]]")

        val ownArgs = withOffsets(q"Offsets.zero") {
          case (name, off) =>
            val nameString = name.decodedName.toString
            val nameConst = Constant(nameString)
            q"""val $name = $self.$name.own(new Field(_term,$self.$name,$off)($nameConst))"""
          //            q"""val $name:$self.$name.Term = ???"""

        }

        val own = q"""
          new OwnedTerm[Value] with Term {
              def self = _term
              override val domain:_dom.type = _dom
              ..$ownArgs
              def copy(args: IndexedSeq[ArgumentType]) = own(args(0))
          }
          """

        val classNameString = Constant(caseClassTypeName.toString)

        val newTerm = q"""
          $caseClassDef
          class $domClassName[..$covariantMergedTypeParameters](..$domConstructorArgs, val valueSemantics:Boolean = true) extends ml.wolfe.term.ProductDom {
            _dom =>

            import ml.wolfe.term._

            override def productName = $classNameString

            type Value = $caseClassTypeName[..$genTypenames]
            type Var = DomVar
            type Term = DomTerm

            def own(_term: TypedTerm[Value]):Term = $own
            val lengths = if (valueSemantics) $lengths else $lengths + Offsets(discOff = 1)
            override val dimensions = $dimensions

            def one = new $caseClassTypeName[..$genTypenames](..$ones)
            def zero = new $caseClassTypeName[..$genTypenames](..$zeros)
            override def sparseZero = new $caseClassTypeName[..$genTypenames](..$sparseZeros)

            $marginalsCaseClassDef

            private var _currentId = 0

            private def nextId() = {
              val oldId = _currentId
              _currentId += 1
              oldId
            }

            def toValue(_setting:Setting,_offsets:Offsets):Value = {
              ..$toValueArgs
              new $caseClassTypeName[..$genTypenames](..$argNames)
            }

            def toMarginals(_Msg:Msg,_offsets:Offsets):Marginals = {
              ..$toMarginalsArgs
              new Marginals(..$argNames)
            }

            def copyValue(_value:Value,_setting:Setting,_offsets:Offsets) {
              ..$copyValueStatements
              if (!valueSemantics) _setting.disc(_offsets.discOff + lengths.discOff - 1) = nextId()
            }

            override def indexOfSetting(setting: Setting) = {
              if (valueSemantics) super.indexOfSetting(setting) else setting.disc(lengths.discOff - 1)
            }

            override def settingOfIndex(index: Int, tgt: Setting) = {
              require(valueSemantics, "Cannot convert indices to values for object semantics")
              super.settingOfIndex(index, tgt)
            }

            def copyMarginals(_marginals: Marginals, _Msg: Msg, _offsets: Offsets) = {
              ..$copyMarginalStatements
            }


            def fillZeroMsg(_target: Msg, _offsets: Offsets) = {
              ..$fillZeroMsgStatements
            }

            def Variable(name: String) = new BaseVar(name) with DomVar {
              ..$staticVarArgs
            }

            def Const(_value: Value) = new DomTermImpl {
              ..$constArgs
              def copy(args:IndexedSeq[ArgumentType]) = Term(..$castArgs)
            }

            def Term(..$termConstructorArgs):DomTermImpl = new DomTermImpl {
              ..$termConstructorDefs
              def copy(args:IndexedSeq[ArgumentType]) = Term(..$castArgs)
            }

            trait DomTermImpl extends DomTerm with super.DomTermImpl { _term =>
              type ArgumentType = ml.wolfe.term.Term[Dom]
              lazy val arguments:IndexedSeq[ml.wolfe.term.Term[Dom]] = IndexedSeq(..$composedArgs)
            }

            trait DomTerm extends super.DomTerm  {
              ..$termArgsDef
            }

            trait DomVar extends super.DomVar with DomTerm {
              _domVar =>
              ..$varArgsDef
            }

          }
          object $caseClassTermName {

            type Dom[..$genTypes] = $domClassName[..$mergedTypeBounds]
            type Term[..$genTypes] = $domClassName[..$mergedTypeBounds]#Term

            class ValuesConstructor[..$genTypes] {
              def apply[..$argDomTypeParameters](implicit ..$domConstructorArgs) =
                new $domClassName[..$mergedSingletonTypes](..$argNames)
            }
            class ObjectsConstructor[..$genTypes] {
              def apply[..$argDomTypeParameters](implicit ..$domConstructorArgs) =
                new $domClassName[..$mergedSingletonTypes](..$argNames,false)
            }
            def Values[..$genTypes] =
              new ValuesConstructor[..$genTypenames]
            def Objects[..$genTypes] =
              new ObjectsConstructor[..$genTypenames]
          }
        """
        //def dom:$domClassName = new $domClassName
        //        println(newTerm)
        //        type Dom[..$mergedTypeParameters] = $domClassName[..$mergedTypesFromTypeParameters]
        //            type Test2[C, Dom_19store <: ml.wolfe.term.TypedDom[IndexedSeq[C]]] = Seq[Double];
        //            type Dom[..typeParameterHack] = $domClassName[..$mergedTypesFromTypeParameters]
        //            type Dom[..typeParameterHack] = Seq[Double]

        //            type Dom[..$typeParameterHack] = $domClassName[..$mergedTypesFromTypeParameters]
        //            type Term[..$typeParameterHack] = ml.wolfe.term.Term[$domClassName[..$mergedTypesFromTypeParameters]]

//        println(newTerm)
        c.Expr[Any](newTerm)
      case _ => c.abort(c.enclosingPosition, "Can't create domain")
    }
  }


}

class domain extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CaseClassDom.implOnClass
}


