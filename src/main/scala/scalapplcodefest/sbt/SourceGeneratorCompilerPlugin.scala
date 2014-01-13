package scalapplcodefest.sbt

import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scala.tools.nsc.Global
import scala.reflect.internal.Phase
import scala.io.Source
import scala.collection.mutable
import java.io.{PrintWriter, File}
import scala.annotation.StaticAnnotation

object SourceGeneratorCompilerPlugin {
  val name = "Wolfe Source Generator"
  val generationPhase = "wolfe generation"
  val collectionPhase = "wolfe collection"

  def compiledTag(originalName: String) = s"""@${classOf[Compiled].getName}("$originalName")"""
  def compiledShortTag(originalName: String) = s"""@${classOf[Compiled].getSimpleName}("$originalName")"""

}

/**
 * @author Sebastian Riedel
 */
class SourceGeneratorCompilerPlugin(val env: GeneratorEnvironment,
                                    targetDir: File,
                                    replacers: List[CodeStringReplacer] = Nil) extends Plugin {
  plugin =>

  import SourceGeneratorCompilerPlugin._

  val name = SourceGeneratorCompilerPlugin.name
  val components = List(DefCollectionComponent, GenerationComponent)
  val description = "Generates optimized scala code"
  val global = env.global

  object DefCollectionComponent extends PluginComponent {
    val global: plugin.env.global.type = env.global
    val phaseName = collectionPhase
    val runsAfter = List("namer")
    def newPhase(prev: Phase) = new CollectionPhase(prev)

    class CollectionPhase(prev: Phase) extends StdPhase(prev) {

      import global._
      import env._

      val traverser = new Traverser {

        private var toCollect = true

        override def traverse(tree: Tree) = tree match {
          case p: PackageDef =>
            toCollect = true
            super.traverse(tree)

          case md@ModuleDef(_, _, _) =>
            if (!md.symbol.hasAnnotation(MarkerCollect) && !md.symbol.hasAnnotation(MarkerCompile)) toCollect = false
            if (toCollect) super.traverse(tree)

          case cd@ClassDef(_, _, _, _) =>
            if (!cd.symbol.hasAnnotation(MarkerCollect) && !cd.symbol.hasAnnotation(MarkerCompile)) toCollect = false
            if (toCollect) super.traverse(tree)

          case dd:DefDef =>
            env.implementations(dd.symbol) = dd
            if (toCollect) super.traverse(tree)

          case vd:ValDef =>
            env.implementations(vd.symbol) = vd
            if (toCollect) super.traverse(tree)

          case _ => if (toCollect) super.traverse(tree)
        }
      }

      def apply(unit: CompilationUnit) = {
        traverser traverse unit.body
      }
    }

  }

  object GenerationComponent extends PluginComponent {
    val global = env.global
    val phaseName = SourceGeneratorCompilerPlugin.generationPhase
    val runsAfter = List(DefCollectionComponent.phaseName)
    var packageName: String = "root"
    def newPhase(prev: scala.tools.nsc.Phase) = new GenerationPhase(prev)

    class GenerationPhase(prev: Phase) extends StdPhase(prev) {
      override def name = plugin.name
      def apply(unit: global.CompilationUnit) = {
        import global._
        val sourceFile = unit.source.file.file
        val sourceText = Source.fromFile(sourceFile).getLines().mkString("\n")
        val modified = new ModifiedSourceText(sourceText)

        //global.treeBrowser.browse(unit.body)
        for (tree <- unit.body) {
          def applyFirstMatchingGenerator(gen: List[CodeStringReplacer]) {
            gen match {
              case Nil =>
              case head :: tail =>
                val changed = head.replace(tree.asInstanceOf[head.env.global.Tree], modified)
                if (!changed) applyFirstMatchingGenerator(tail)
            }
          }

          tree match {
            case PackageDef(ref, _) =>
              packageName = sourceText.substring(ref.pos.start, ref.pos.end)
              modified.replace(ref.pos.start, ref.pos.end, packageName + ".compiled")
              modified.insert(ref.pos.end, s"\n\nimport ${classOf[Compiled].getName}")
              modified.insert(ref.pos.end, s"\n\nimport $packageName._")


            case md: ModuleDef =>
              modified.insert(md.pos.start, compiledShortTag(md.symbol.fullName('.')) + " ")

            case other =>
              applyFirstMatchingGenerator(replacers)
          }
        }

        println(modified.current())
        val modifiedDirName = packageName.replaceAll("\\.", "/") + "/compiled"
        val modifiedDir = new File(targetDir, modifiedDirName)
        modifiedDir.mkdirs()
        val modifiedFile = new File(modifiedDir, sourceFile.getName)
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

  val implementations = new mutable.HashMap[Any, ValOrDefDef]()

  val MarkerCollect = rootMirror.getClassByName(newTermName(classOf[Collect].getName))
  val MarkerCompile = rootMirror.getClassByName(newTermName(classOf[Compile].getName))

  val betaReducer = new BetaReducer
  val valInliner = new ValInliner

  def betaReduce(tree:Tree) = betaReducer.transform(tree)
  def inlineVals(tree:Tree) = valInliner.transform(tree)

  class ValInliner extends Transformer {
    override def transform(tree: Tree) = tree match {
      case i@Ident(name) => implementations.get(i.symbol) match {
        case Some(ValDef(_,_,_,rhs)) => rhs
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
    }
  }

  class BetaReducer extends Transformer {
    override def transform(tree: Tree) = tree match {
      case Apply(f,args) => implementations.get(f.symbol) match {
        case Some(DefDef(_,_,_,List(defArgs),_,rhs)) =>
          //replace arguments of defdef with arguments of apply
          val binding = (defArgs.map(_.symbol) zip args).toMap
          val substituter = new Substituter(binding)
          val result = substituter transform rhs
          result
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
    }
  }

  class Substituter(binding:Map[Symbol,Tree]) extends Transformer {
    override def transform(tree: Tree) = tree match {
      case i:Ident => binding.get(i.symbol) match {
        case Some(value) => value
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
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
class Collect extends StaticAnnotation


/**
 * Maintains a mapping from original character offsets to offsets in a modified string.
 * @param original the string to modify.
 */
class ModifiedSourceText(original: String, offset: Int = 0) {
  private val source = new StringBuilder(original)
  private val originalToModified = new mutable.HashMap[Int, Int]()

  for (i <- 0 until original.length) originalToModified(i) = i - offset

  def insert(start: Int, text: String) {
    source.insert(originalToModified(start), text)
    for (i <- start until original.length) originalToModified(i) += text.length
  }
  def replace(start: Int, end: Int, text: String) {
    source.replace(originalToModified(start), originalToModified(end), text)
    val offset = -(end - start) + text.length
    for (i <- start until original.length) originalToModified(i) += offset
  }

  def current() = source.toString()

}

object ModifiedSourceText {

  def fromTree(tree: Global#Tree) = {

  }

}


