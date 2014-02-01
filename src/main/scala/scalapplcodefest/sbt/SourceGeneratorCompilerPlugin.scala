package scalapplcodefest.sbt

import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scala.tools.nsc.Global
import scala.reflect.internal.Phase
import scala.io.Source
import scala.collection.mutable
import java.io.{PrintWriter, File}
import scala.annotation.StaticAnnotation
import scala.Predef._
import scala.Some

object SourceGeneratorCompilerPlugin {
  val name            = "Wolfe Source Generator"
  val generationPhase = "wolfe generation"
  val collectionPhase = "wolfe collection"
  val namePostfix     = "Compiled"

  def compiledTag(originalName: String) = s"""${classOf[Compiled].getName}("$originalName")"""
  def compiledShortTag(originalName: String) = s"""${classOf[Compiled].getSimpleName}("$originalName")"""

}

/**
 * @author Sebastian Riedel
 */
class SourceGeneratorCompilerPlugin(val env: GeneratorEnvironment,
                                    targetDir: File,
                                    replacers: List[CodeStringReplacer] = Nil) extends Plugin {
  plugin =>

  import SourceGeneratorCompilerPlugin._

  val name        = SourceGeneratorCompilerPlugin.name
  val components  = List(DefCollectionComponent, GenerationComponent)
  val description = "Generates optimized scala code"
  val global      = env.global

  object DefCollectionComponent extends PluginComponent {
    val global: plugin.env.global.type = env.global
    val phaseName                      = collectionPhase
    val runsAfter                      = List("namer")
    def newPhase(prev: Phase) = new CollectionPhase(prev)

    class CollectionPhase(prev: Phase) extends StdPhase(prev) {

      import global._
      import env._

      val collector = new Traverser {

        private var toCollect = true

        override def traverse(tree: Tree) = tree match {
          case p: PackageDef =>
            toCollect = true
            super.traverse(tree)

          case md@ModuleDef(_, _, _) =>
            env.moduleDefs(md.symbol) = md
            if (!md.symbol.hasAnnotation(MarkerAnalyze) && !md.symbol.hasAnnotation(MarkerCompile)) toCollect = false
            if (toCollect) super.traverse(tree)

          case cd@ClassDef(_, _, _, _) =>
            env.classDefs(cd.symbol) = cd
            if (!cd.symbol.hasAnnotation(MarkerAnalyze) && !cd.symbol.hasAnnotation(MarkerCompile)) toCollect = false
            if (toCollect) super.traverse(tree)

          case dd: DefDef if dd.vparamss.isEmpty =>
            //we treat methods without parameters as values
            val vd = ValDef(dd.symbol, dd.rhs)
            env.valOrDefDefs(dd.symbol) = vd
            if (toCollect) super.traverse(tree)

          case dd: DefDef =>
            env.valOrDefDefs(dd.symbol) = dd
            if (toCollect) super.traverse(tree)

          case vd: ValDef =>
            env.valOrDefDefs(vd.symbol) = vd
            if (toCollect) super.traverse(tree)

          case _ => if (toCollect) super.traverse(tree)
        }
      }

      def apply(unit: CompilationUnit) = {
        collector traverse unit.body
      }
    }

  }

  object GenerationComponent extends PluginComponent {
    val global     : env.global.type = env.global
    val phaseName                    = SourceGeneratorCompilerPlugin.generationPhase
    val runsAfter                    = List(DefCollectionComponent.phaseName)
    var packageName: String          = "root"
    def newPhase(prev: scala.tools.nsc.Phase) = new GenerationPhase(prev)

    class GenerationPhase(prev: Phase) extends StdPhase(prev) {
      override def name = plugin.name
      def apply(unit: global.CompilationUnit) = {
        import env.global._
        val sourceFile = unit.source.file.file
        val sourceText = Source.fromFile(sourceFile).getLines().mkString("\n")
        val modified = new ModifiedSourceText(sourceText)

        val modifier = new Traverser {

          override def traverse(tree: Tree) = {
            def applyFirstMatchingGenerator(gen: List[CodeStringReplacer]) {
              gen match {
                case Nil =>
                case head :: tail =>
                  val changed = head.replace(tree.asInstanceOf[head.env.global.Tree], modified)
                  if (!changed) applyFirstMatchingGenerator(tail)
              }
            }
            applyFirstMatchingGenerator(replacers)
            super.traverse(tree)
          }
        }

        val selector = new Traverser {
          override def traverse(tree: Tree) = tree match {
            case PackageDef(ref, _) =>
              packageName = sourceText.substring(ref.pos.start, ref.pos.end)
              modified.insert(ref.pos.end, s"\n\nimport ${classOf[Compiled].getName}")
              super.traverse(tree)

            case cd: ClassDef =>
              if (cd.symbol.hasAnnotation(env.MarkerCompile)) {
                for (ann <- cd.symbol.annotations; if ann.matches(env.MarkerCompile))
                  modified.replace(ann.pos.start, ann.pos.end, compiledShortTag(cd.symbol.fullName('.')))
                modified.insert(cd.impl.pos.start - 1, namePostfix)
                modifier traverse cd
              }
              else {
                for (ann <- cd.symbol.annotations) modified.delete(ann.pos.start - 1, ann.pos.end)
                modified.delete(cd.pos.start, cd.pos.end)
              }


            case md: ModuleDef =>
              if (md.symbol.hasAnnotation(env.MarkerCompile)) {
                for (ann <- md.symbol.annotations; if ann.matches(env.MarkerCompile))
                  modified.replace(ann.pos.start, ann.pos.end, compiledShortTag(md.symbol.fullName('.')))
                modified.insert(md.pos.end, namePostfix)
                modifier traverse md
              }
              else {
                for (ann <- md.symbol.annotations) modified.delete(ann.pos.start - 1, ann.pos.end)
                modified.delete(md.pos.start, md.pos.end)
              }

            case _ => super.traverse(tree)
          }

        }


        //global.treeBrowser.browse(unit.body)
        selector traverse unit.body

        println(modified.current())
        val modifiedDirName = packageName.replaceAll("\\.", "/")
        val modifiedDir = new File(targetDir, modifiedDirName)
        modifiedDir.mkdirs()
        val sourceFileName = sourceFile.getName
        val modifiedFile = new File(modifiedDir, sourceFileName.substring(0, sourceFileName.lastIndexOf('.')) + "Compiled.scala")
        val out = new PrintWriter(modifiedFile)
        out.println(modified.current())
        out.close()
      }
    }

  }

}

trait InCompilerPlugin {
  val global: Global
  def definitionOf(sym: global.Symbol): Option[global.Tree] = None
}

trait InGeneratorEnvironment {
  val env: GeneratorEnvironment
}

class GeneratorEnvironment(val global: Global) {

  import global._

  val valOrDefDefs = new mutable.HashMap[Symbol, ValOrDefDef]()
  val moduleDefs   = new mutable.HashMap[Symbol, ModuleDef]()
  val classDefs    = new mutable.HashMap[Symbol, ClassDef]()


  val MarkerAnalyze = rootMirror.getClassByName(newTermName(classOf[Analyze].getName))
  val MarkerCompile = rootMirror.getClassByName(newTermName(classOf[Compile].getName))

  val blockSimplifier = new BlockSimplifier
  val betaReducer     = new BetaReducer
  val valInliner      = new ValInliner
  val methodReplacer  = new ReplaceMethodsWithFunctions

  def normalize(text: String) = text.replaceAll("\\.this\\.", ".").replaceAll("`package`.", "").replaceAll("\\(Predef\\.implicitly\\)", "")

  def simplifyBlocks(tree: Tree) = blockSimplifier.transform(tree)
  def betaReduce(tree: Tree) = betaReducer.transform(tree)
  def inlineVals(tree: Tree) = valInliner.transform(tree)
  def replaceMethods(tree: Tree) = methodReplacer.transform(tree)
  def transform(tree: Tree, pf: PartialFunction[Tree, Tree]) = new TransformWithPartialFunction(pf).transform(tree)
  def substitute(tree: Tree, binding: Map[Symbol, Tree]) = {
    val substituter = new Substituter(binding)
    substituter transform tree
  }

  class BlockSimplifier extends Transformer {
    override def transform(tree: Tree) = tree match {
      case Block(Nil, expr) => super.transform(expr)
      case _ => super.transform(tree)
    }
  }

  class ValInliner extends Transformer {
    override def transform(tree: Tree) = tree match {
      case i@Ident(name) => valOrDefDefs.get(i.symbol) match {
        case Some(ValDef(_, _, _, rhs)) => rhs
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
    }
  }

  class ReplaceMethodsWithFunctions extends Transformer {
    def getDef(f: Tree) = f match {
      case TypeApply(templateFun, _) => valOrDefDefs.get(templateFun.symbol)
      case _ => valOrDefDefs.get(f.symbol)
    }

    def createFunction(defArgs: List[List[ValDef]], rhs: Tree): Function = defArgs match {
      case Nil => Function(Nil, rhs)
      case headArgs :: Nil => Function(headArgs, rhs)
      case headArgs :: tail => Function(headArgs, createFunction(tail, rhs))
    }

    override def transform(tree: Tree): Tree = tree match {
      case TypeApply(f@Ident(_), _) => getDef(f) match {
        case Some(DefDef(_, _, _, defArgs, _, rhs)) => createFunction(defArgs, transform(rhs))
        case _ => super.transform(tree)
      }
      case f@Ident(_) => getDef(f) match {
        case Some(DefDef(_, _, _, defArgs, _, rhs)) => createFunction(defArgs, transform(rhs))
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
    }
  }

  //max[Double,T] = ... becomes Function(  

  class BetaReducer extends Transformer {

    def substitute(defArgs: List[ValDef], args: List[Tree], tree: Tree): Tree = {
      val binding = (defArgs.map(_.symbol) zip args).toMap
      val substituter = new Substituter(binding)
      val result = substituter transform tree
      result
    }

    def getDefDef(f: Tree) = f match {
      case TypeApply(templateFun, _) => valOrDefDefs.get(templateFun.symbol)
      case _ => valOrDefDefs.get(f.symbol)
    }

    override def transform(tree: Tree): Tree = {
      val transformed = super.transform(tree)
      transformed match {
        case Apply(Function(defArgs, rhs), args) => substitute(defArgs, args, rhs)
        case other => other
      }
    }
  }

  class Substituter(binding: Map[Symbol, Tree]) extends Transformer {
    override def transform(tree: Tree) = tree match {
      case i: Ident => binding.get(i.symbol) match {
        case Some(value) => value
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
    }
  }

  class TransformWithPartialFunction(pf: PartialFunction[Tree, Tree]) extends Transformer {
    override def transform(tree: Tree) = {
      val transformed = super.transform(tree)
      if (pf.isDefinedAt(transformed)) pf(transformed) else transformed
    }

  }


  class SimplePrinter(out: PrintWriter) extends TreePrinter(out) {
    override def printTree(tree: Tree) = tree match {
      case s@Select(qualifier, name) =>
        //        System.out.println(s"Select: ${s.toString()}")
        super.printTree(tree)
      case _ => super.printTree(tree)
    }

  }

}

trait CodeStringReplacer extends InGeneratorEnvironment {
  def replace(tree: env.global.Tree, modification: ModifiedSourceText): Boolean

}

/**
 * Indicates wolfe compiled code.
 * @param original the original symbol the annotated symbol is a compilation of.
 */
class Compiled(original: String) extends StaticAnnotation

/**
 * Annotates classes that should be wolfe-compiled
 */
class Compile extends StaticAnnotation


/**
 * Annotates classes from which we should collect definitions that will be wolfe-compiled
 * when part of classes with @Compile annotation.
 */
class Analyze extends StaticAnnotation


/**
 * Maintains a mapping from original character offsets to offsets in a modified string.
 * @param original the string to modify.
 */
class ModifiedSourceText(original: String, offset: Int = 0) {
  private val source             = new StringBuilder(original)
  private val originalToModified = new mutable.HashMap[Int, Int]()

  for (i <- 0 to original.length) originalToModified(i) = i - offset

  def insert(start: Int, text: String) {
    source.insert(originalToModified(start), text)
    for (i <- start to original.length) originalToModified(i) += text.length
  }
  def replace(start: Int, end: Int, text: String) {
    source.replace(originalToModified(start), originalToModified(end), text)
    val offset = -(end - start) + text.length
    for (i <- start to original.length) originalToModified(i) += offset
  }

  def delete(start: Int, end: Int) = replace(start, end, "")

  def indentationOfLineAt(position: Int) = {
    var current = originalToModified(position)
    var lastNonWhiteSpace = current
    while (current > 0 && source.charAt(current) != '\n') {
      current = current - 1
      if (!source.charAt(current).isWhitespace) lastNonWhiteSpace = current
    }
    lastNonWhiteSpace - current - 1
  }

  def current() = source.toString()

}

object ModifiedSourceText {

  def fromTree(tree: Global#Tree) = {

  }

}


