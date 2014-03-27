package ml.wolfe.macros

import scala.reflect.macros.Context
import scala.collection.mutable
import scala.language.experimental.macros


trait MetaStructures[C <: Context] extends CodeRepository[C]
                                           with MetaCaseClassStructures[C]
                                           with MetaFunStructures[C]
                                           with MetaAtomicStructures[C]
                                           with MetaSeqStructures[C]
                                           with PatternRepository[C] {

  import context.universe._

  /**
   * An expression pointing to a structure.
   * @param structure the expression that refers to a structure.
   * @param meta the meta structure corresponding to the structure pointed to.
   */
  case class StructurePointer(structure: Tree, meta: MetaStructure)

  /**
   * Represents code that generates structures for a given sample space.
   *
   * @author Sebastian Riedel
   */
  trait MetaStructure {

    /**
     * @return The name of the structure class.
     */
    def className: TypeName

    /**
     * @return A list of definitions of domain values needed in the structure generation code.
     */
    def domainDefs: List[ValDef]

    /**
     * Creates the code the defines the structure class.
     * @param graphName the structure class needs an underlying factor graph, and this parameter provides its name.
     *                  This means that when instantiating the structure class a factor graph with the given name needs
     *                  to be in scope.
     * @return code that defines the structure class.
     */
    def classDef(graphName: TermName): ClassDef

    /**
     * @return the type of objects this structure represents (i.e. the type of objects in the sample space).
     */
    def argType: Type

    /**
     * @return any meta structure used within this meta structure.
     */
    def children: List[MetaStructure]

    /**
     * @return this and all descendant metastructures.
     */
    def all: List[MetaStructure] = this :: children.flatMap(_.all)

    /**
     * A matcher takes a tree and checks whether the tree corresponds to any sub-structure within this
     * structure.
     * @param root the root matcher determines matching of the top-level structure.
     * @return a matcher.
     */
    def matcher(root: Tree => Option[StructurePointer]): Tree => Option[StructurePointer] = matcher(root, root)

    /**
     * A matcher takes a tree and returns a tree that represents the sub-structure that corresponds to the
     * tree, if any. This method creates matchers for the structures of this meta-structure.
     * @param parent a matcher for the parent structure.
     * @param result the current result matcher.
     * @return a matcher...
     */
    def matcher(parent: Tree => Option[StructurePointer], result: Tree => Option[StructurePointer]): Tree => Option[StructurePointer]

  }


  /**
   * get all structures in expression.
   * @param tree the expression to search for structures in.
   * @param matchStructure the matcher to apply on sub-trees to
   * @return all structures in expression `tree`.
   */
  def structures(tree: Tree, matchStructure: Tree => Option[StructurePointer]): List[Tree] = {
    var result: List[Tree] = Nil
    val traverser = new Traverser with WithFunctionStack {
      override def traverse(tree: Tree) = {
        pushIfFunction(tree)
        val tmp = matchStructure(tree) match {
          case Some(structure) if !hasFunctionArgument(tree) =>
            result ::= structure.structure
          case _ =>
            super.traverse(tree)
        }
        popIfFunction(tree)
        tmp
      }
    }
    traverser traverse tree
    result
  }

  /**
   * Takes an expression and replaces internal expressions `expr` with
   * `structure.value` if `expr` corresponds to `structure`.
   * @param tree the expression to inject structure into.
   * @param matcher the matcher to look for structure with.
   * @return the tree with injected structure.
   */
  def injectStructure(tree: Tree, matcher: Tree => Option[StructurePointer]) = {
    val transformer = new Transformer {
      val functionStack = new mutable.Stack[Function]()
      override def transform(tree: Tree) = {
        tree match {
          case f: Function => functionStack.push(f)
          case _ =>
        }
        val result = matcher(tree) match {
          case Some(structure) => {
            //get symbols in tree
            val symbols = tree.collect({case i: Ident => i}).map(_.name).toSet //todo: this shouldn't just be by name
            val hasFunctionArg = functionStack.exists(_.vparams.exists(p => symbols(p.name)))
            if (hasFunctionArg)
              super.transform(tree)
            else
              q"${structure.structure}.value()"
          }
          case _ => super.transform(tree)
        }
        tree match {
          case _: Function => functionStack.pop
          case _ =>
        }
        result
      }
    }
    transformer transform tree
  }

  /**
   * @param args a list of expressions corresponding to structures.
   * @param block the code that should be executed in the loop.
   * @return a code block that executes `block` for every setting of the given structure `args`.
   */
  def loopSettings(args: List[Tree])(block: Tree): Tree = args match {
    case Nil => block
    case head :: tail =>
      val inner = loopSettings(tail)(block)
      q"{ $head.resetSetting();  while ($head.hasNextSetting) {  $head.nextSetting(); $inner } }"
  }


  /**
   * Creates a matcher that checks for an identifier matching the given symbol, and then returns the
   * given root structure.
   * @param rootArgument the symbol of the variable to replace.
   * @param rootStructure the structure to replace the variable with.
   * @return a matcher.
   */
  def rootMatcher(rootArgument: Symbol, rootStructure: Tree, rootMetaStructure: MetaStructure): Tree => Option[StructurePointer] = {
    val root = (tree: Tree) => tree match {
      case i: Ident if i.symbol == rootArgument => Some(StructurePointer(rootStructure, rootMetaStructure))
      case _ => None
    }
    root
  }


  /**
   * @param iterable a tree representing an iterable object
   * @return the argument type of the iterable, e.g. for Seq(1,2,3) it would be Int.
   */
  def iterableArgumentType(iterable: Tree): Type = {
    val iterableType = iterable.tpe.baseType(scalaSymbols.iterableClass)
    val TypeRef(_, _, List(argType)) = iterableType
    argType
  }

  /**
   * Creates a meta structure for the given sample space.
   * @param sampleSpace the sample space to create a meta structure for.
   * @return the meta structure for the given sample space.
   */
  def metaStructure(sampleSpace: Tree): MetaStructure = {
    //todo: assert that domain is an iterable
    //require(sampleSpace.tpe <:< scalaSymbols.iterableClass.typeSignature)
    sampleSpace match {
      case q"$all[${_},$caseClassType]($unwrap[..${_}]($constructor))($cross(..$sets))"
        if all.symbol == wolfeSymbols.all && wolfeSymbols.unwraps(unwrap.symbol) && wolfeSymbols.crossProducts(cross.symbol) =>
        metaCaseClassStructure(constructor, sets, caseClassType)
      case q"$all[${_},$caseClassType]($constructor)(..$sets)"
        if all.symbol == wolfeSymbols.all =>
        metaCaseClassStructure(constructor, sets, caseClassType)
      case q"$pred[${_}]($keyDom)" if pred.symbol == wolfeSymbols.Pred =>
        val valueDom = context.typeCheck(q"ml.wolfe.Wolfe.bools")
        metaFunStructure(sampleSpace, keyDom, valueDom)
      case q"$seq($size,$dom)" if wolfeSymbols.fixedLengthSeqs == seq.symbol =>
        val typeOfArg = iterableArgumentType(sampleSpace)
        val elementMeta = metaStructure(dom)
        new MetaSeqStructure {
          def elementMetaStructure = elementMeta
          def argType = typeOfArg
          def lengthInitializer = q"setLength($size)"
        }
      case _ =>
        inlineOnce(sampleSpace) match {
          case Some(inlined) => metaStructure(inlined)
          case None =>
            new MetaAtomicStructure {
              def domain = sampleSpace
            }
        }
    }
  }

  def metaCaseClassStructure(constructor: Tree, sets: List[Tree], caseClassType: Tree) = {
    val applySymbol = constructor.tpe.member(newTermName("apply")).asMethod
    val args = applySymbol.paramss.head
    new MetaCaseClassStructure {
      val tpe             = caseClassType.tpe
      val fieldStructures = sets.map(metaStructure(_))
      val fields          = args
    }
  }

  def metaFunStructure(sampleSpace: Tree, keyDom: Tree, valueDom: Tree) = {
    val keyDomains = keyDom match {
      case q"$cross[..${_}](..$doms)" if wolfeSymbols.crosses(cross.symbol) => doms
      case _ => List(keyDom)
    }
    val typeOfArg = iterableArgumentType(sampleSpace)
    val valueStructure = metaStructure(valueDom)
    new MetaFunStructure {
      def argType = typeOfArg
      def valueMetaStructure = valueStructure
      def keyDoms = keyDomains
    }
  }

}


object MetaStructure {


  /**
   * Useful for unit tests.
   * @param sampleSpace the sample space to inspect.
   * @tparam T type of objects in sample space.
   * @return a structure corresponding to the given sample space.
   */
  def structure[T](sampleSpace: Iterable[T]): Structure[T] = macro structureImpl[T]

  def projection[T1, T2](sampleSpace: Iterable[T1],
                         projection: T1 => T2): (Structure[T1], Structure[T1] => T2) = macro projectionImpl[T1, T2]

  def projectionImpl[T1: c.WeakTypeTag, T2: c.WeakTypeTag](c: Context)
                                                          (sampleSpace: c.Expr[Iterable[T1]],
                                                           projection: c.Expr[T1 => T2]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with MetaStructures[c.type]
    val meta = helper.metaStructure(sampleSpace.tree)
    val graphName = newTermName("_graph")
    val structName = newTermName("structure")
    val structArgName = newTermName("structArg")
    val cls = meta.classDef(graphName)
    val q"($arg) => $rhs" = projection.tree
    val root = helper.rootMatcher(arg.symbol, q"$structArgName.asInstanceOf[${meta.className}]", meta)
    val injectedRhs = helper.injectStructure(rhs, meta.matcher(root))

    val injectedProj = q"($structArgName:ml.wolfe.macros.Structure[${meta.argType}]) => $injectedRhs"
    val code = q"""
      val $graphName = new ml.wolfe.MPGraph
      $cls
      val $structName = new ${meta.className}
      $graphName.setupNodes()
      ($structName,$injectedProj)
    """
    c.Expr[(Structure[T1], Structure[T1] => T2)](code)
  }


  def structureImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]]): c.Expr[Structure[T]] = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with MetaStructures[c.type]
    val meta = helper.metaStructure(sampleSpace.tree)
    val graphName = newTermName("_graph")
    val cls = meta.classDef(graphName)
    val code = q"""
      val $graphName = new ml.wolfe.MPGraph
      $cls
      val structure = new ${meta.className}
      $graphName.setupNodes()
      structure
    """
    c.Expr[Structure[T]](code)
  }

}

