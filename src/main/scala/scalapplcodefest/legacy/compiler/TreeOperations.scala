package scalapplcodefest.legacy.compiler

import scala.tools.nsc.Global

/**
 * User: rockt
 * Date: 1/3/14
 * Time: 11:44 AM
 */

object TreeOperations {
  def noopTemplate(global: Global)(tree: global.Tree): global.Tree = {
    import global._

    tree match {
      case Ident(name) =>
        Ident(name)
      case Select(qualifier, selector) =>
        Select(qualifier, selector)
      case Apply(fun, args) =>
        Apply(fun, args)
      case TypeTree() =>
        TypeTree() //???
      case Literal(value) =>
        Literal(value)
      case This(qual) =>
        This(qual)
      case ValDef(mods, name, tpt, rhs) =>
        ValDef(mods, name, tpt, rhs)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        DefDef(mods, name, tparams, vparamss, tpt, rhs)
      case Block(stats, expr) =>
        Block(stats, expr)
      case If(cond, thenp, elsep) =>
        If(cond, thenp, elsep)
      case CaseDef(pat, guard, body) =>
        CaseDef(pat, guard, body)
      case TypeApply(fun, args) =>
        TypeApply(fun, args)
      case AppliedTypeTree(tpt, args) =>
        AppliedTypeTree(tpt, args)
      case Bind(name, body) =>
        Bind(name, body)
      case Function(vparams, body) =>
        Function(vparams, body)
      case Match(selector, cases) =>
        Match(selector, cases)
      case New(tpt) =>
        New(tpt)
      case Assign(lhs, rhs) =>
        Assign(lhs, rhs)
      case AssignOrNamedArg(lhs, rhs) =>
        AssignOrNamedArg(lhs, rhs)
      case Try(block, catches, finalizer) =>
        Try(block, catches, finalizer)
      case EmptyTree =>
        tree
      case Throw(expr) =>
        Throw(expr)
      case Super(qual, mix) =>
        Super(qual, mix)
      case TypeBoundsTree(lo, hi) =>
        TypeBoundsTree(lo, hi)
      case Typed(expr, tpt) =>
        Typed(expr, tpt)
      case Import(expr, selectors) =>
        Import(expr, selectors)
      case Template(parents, self, body) =>
        Template(parents, self, body)
      case ClassDef(mods, name, tparams, impl) =>
        ClassDef(mods, name, tparams, impl)
      case ModuleDef(mods, name, impl) =>
        ModuleDef(mods, name, impl)
      case TypeDef(mods, name, tparams, rhs) =>
        TypeDef(mods, name, tparams, rhs)
      case LabelDef(name, params, rhs) =>
        LabelDef(name, params, rhs)
      case PackageDef(pid, stats) =>
        PackageDef(pid, stats)
      case Annotated(annot, arg) =>
        Annotated(annot, arg)
      case SingletonTypeTree(ref) =>
        SingletonTypeTree(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        SelectFromTypeTree(qualifier, selector)
      case CompoundTypeTree(templ) =>
        CompoundTypeTree(templ)
      case ExistentialTypeTree(tpt, whereClauses) =>
        ExistentialTypeTree(tpt, whereClauses)
      case Return(expr) =>
        Return(expr)
      case Alternative(trees) =>
        Alternative(trees)
      case Star(elem) =>
        Star(elem)
      case UnApply(fun, args) =>
        UnApply(fun, args)
      case ArrayValue(elemtpt, trees) =>
        ArrayValue(elemtpt, trees)
      case ApplyDynamic(qual, args) =>
        ApplyDynamic(qual, args)
      case ReferenceToBoxed(idt) =>
        ReferenceToBoxed(idt)
      case _ => tree //FIXME
    }
  }

  //traversing
  def itraverse(global: Global)(traverser: global.Traverser, tree: global.Tree): Unit = {
    def mclass(sym: global.Symbol) = sym map (_.asModule.moduleClass)
    import traverser._
    import global._

    tree match {
      case EmptyTree =>
        ;
      case PackageDef(pid, stats) =>
        traverse(pid)
        atOwner(mclass(tree.symbol)) {
          traverseTrees(stats)
        }
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(impl)
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(mclass(tree.symbol)) {
          traverseTrees(mods.annotations); traverse(impl)
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverse(tpt); traverse(rhs)
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverseTreess(vparamss); traverse(tpt); traverse(rhs)
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(rhs)
        }
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case Annotated(annot, arg) =>
        traverse(annot); traverse(arg)
      case Template(parents, self, body) =>
        traverseTrees(parents)
        if (!self.isEmpty) traverse(self)
        traverseStats(body, tree.symbol)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Star(elem) =>
        traverse(elem)
      case Bind(name, body) =>
        traverse(body)
      case UnApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ArrayValue(elemtpt, trees) =>
        traverse(elemtpt); traverseTrees(trees)
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          traverseTrees(vparams); traverse(body)
        }
      case Assign(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case AssignOrNamedArg(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case If(cond, thenp, elsep) =>
        traverse(cond); traverse(thenp); traverse(elsep)
      case Match(selector, cases) =>
        traverse(selector); traverseTrees(cases)
      case Return(expr) =>
        traverse(expr)
      case Try(block, catches, finalizer) =>
        traverse(block); traverseTrees(catches); traverse(finalizer)
      case Throw(expr) =>
        traverse(expr)
      case New(tpt) =>
        traverse(tpt)
      case Typed(expr, tpt) =>
        traverse(expr); traverse(tpt)
      case TypeApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case Apply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ApplyDynamic(qual, args) =>
        traverse(qual); traverseTrees(args)
      case Super(qual, _) =>
        traverse(qual)
      case This(_) =>
        ;
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case Ident(_) =>
        ;
      case ReferenceToBoxed(idt) =>
        traverse(idt)
      case Literal(_) =>
        ;
      case TypeTree() =>
        ;
      case SingletonTypeTree(ref) =>
        traverse(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        traverse(qualifier)
      case CompoundTypeTree(templ) =>
        traverse(templ)
      case AppliedTypeTree(tpt, args) =>
        traverse(tpt); traverseTrees(args)
      case TypeBoundsTree(lo, hi) =>
        traverse(lo); traverse(hi)
      case ExistentialTypeTree(tpt, whereClauses) =>
        traverse(tpt); traverseTrees(whereClauses)
      case _ => //xtraverse(traverser, tree) //FIXME
    }
  }

  //OPT ordered according to frequency to speed it up.
  def itransform(global: Global)(transformer: global.Transformer, tree: global.Tree): global.Tree = {
    def mclass(sym: global.Symbol) = sym map (_.asModule.moduleClass)
    import transformer._
    import global._

    val currentOwner: global.Symbol = definitions.RootClass

    val treeCopy = transformer.treeCopy

    // begin itransform
    tree match {
      case Ident(name) =>
        treeCopy.Ident(tree, name)
      case Select(qualifier, selector) =>
        treeCopy.Select(tree, transform(qualifier), selector)
      case Apply(fun, args) =>
        treeCopy.Apply(tree, transform(fun), transformTrees(args))
      case TypeTree() =>
        treeCopy.TypeTree(tree)
      case Literal(value) =>
        treeCopy.Literal(tree, value)
      case This(qual) =>
        treeCopy.This(tree, qual)
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.ValDef(tree, transformModifiers(mods),
            name, transform(tpt), transform(rhs))
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.DefDef(tree, transformModifiers(mods), name,
            transformTypeDefs(tparams), transformValDefss(vparamss),
            transform(tpt), transform(rhs))
        }
      case Block(stats, expr) =>
        treeCopy.Block(tree, transformStats(stats, currentOwner), transform(expr))
      case If(cond, thenp, elsep) =>
        treeCopy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case CaseDef(pat, guard, body) =>
        treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case TypeApply(fun, args) =>
        treeCopy.TypeApply(tree, transform(fun), transformTrees(args))
      case AppliedTypeTree(tpt, args) =>
        treeCopy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          treeCopy.Function(tree, transformValDefs(vparams), transform(body))
        }
      case Match(selector, cases) =>
        treeCopy.Match(tree, transform(selector), transformCaseDefs(cases))
      case New(tpt) =>
        treeCopy.New(tree, transform(tpt))
      case Assign(lhs, rhs) =>
        treeCopy.Assign(tree, transform(lhs), transform(rhs))
      case AssignOrNamedArg(lhs, rhs) =>
        treeCopy.AssignOrNamedArg(tree, transform(lhs), transform(rhs))
      case Try(block, catches, finalizer) =>
        treeCopy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case EmptyTree =>
        tree
      case Throw(expr) =>
        treeCopy.Throw(tree, transform(expr))
      case Super(qual, mix) =>
        treeCopy.Super(tree, transform(qual), mix)
      case TypeBoundsTree(lo, hi) =>
        treeCopy.TypeBoundsTree(tree, transform(lo), transform(hi))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), transform(tpt))
      case Import(expr, selectors) =>
        treeCopy.Import(tree, transform(expr), selectors)
      case Template(parents, self, body) =>
        treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body, tree.symbol))
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          treeCopy.ClassDef(tree, transformModifiers(mods), name,
            transformTypeDefs(tparams), transformTemplate(impl))
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(mclass(tree.symbol)) {
          treeCopy.ModuleDef(tree, transformModifiers(mods),
            name, transformTemplate(impl))
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.TypeDef(tree, transformModifiers(mods), name,
            transformTypeDefs(tparams), transform(rhs))
        }
      case LabelDef(name, params, rhs) =>
        treeCopy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LamdaLifter.proxy'
      case PackageDef(pid, stats) =>
        treeCopy.PackageDef(
          tree, transform(pid).asInstanceOf[RefTree],
          atOwner(mclass(tree.symbol)) {
            transformStats(stats, currentOwner)
          }
        )
      case Annotated(annot, arg) =>
        treeCopy.Annotated(tree, transform(annot), transform(arg))
      case SingletonTypeTree(ref) =>
        treeCopy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        treeCopy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        treeCopy.CompoundTypeTree(tree, transformTemplate(templ))
      case ExistentialTypeTree(tpt, whereClauses) =>
        treeCopy.ExistentialTypeTree(tree, transform(tpt), transformTrees(whereClauses))
      case Return(expr) =>
        treeCopy.Return(tree, transform(expr))
      case Alternative(trees) =>
        treeCopy.Alternative(tree, transformTrees(trees))
      case Star(elem) =>
        treeCopy.Star(tree, transform(elem))
      case UnApply(fun, args) =>
        treeCopy.UnApply(tree, fun, transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
      case ArrayValue(elemtpt, trees) =>
        treeCopy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case ApplyDynamic(qual, args) =>
        treeCopy.ApplyDynamic(tree, transform(qual), transformTrees(args))
      case ReferenceToBoxed(idt) =>
        treeCopy.ReferenceToBoxed(tree, transform(idt) match { case idt1: Ident => idt1 })
      case _ => ??? //xtransform(transformer, tree) //FIXME
    }
  }

  def xtransform(global: Global)(transformer: global.Transformer, tree: global.Tree): global.Tree = {
    import global._

    tree match {
      case DocDef(comment, definition) =>
        transformer.treeCopy.DocDef(tree, comment, transformer.transform(definition))
      case SelectFromArray(qualifier, selector, erasure) =>
        transformer.treeCopy.SelectFromArray(
          tree, transformer.transform(qualifier), selector, erasure)
      case InjectDerivedValue(arg) =>
        transformer.treeCopy.InjectDerivedValue(
          tree, transformer.transform(arg))
      case TypeTreeWithDeferredRefCheck() =>
        transformer.treeCopy.TypeTreeWithDeferredRefCheck(tree)
    }
  }
}