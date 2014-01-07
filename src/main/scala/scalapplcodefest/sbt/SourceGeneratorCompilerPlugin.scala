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
  val phase = "wolfe generation"

  def compiledTag(originalName: String) = s"""@${classOf[Compiled].getName}("$originalName")"""
  def compiledShortTag(originalName: String) = s"""@${classOf[Compiled].getSimpleName}("$originalName")"""

}

/**
 * @author Sebastian Riedel
 */
class SourceGeneratorCompilerPlugin(val global: Global,
                                    targetDir: File,
                                    replacers: List[CodeStringReplacer] = Nil) extends Plugin {
  plugin =>

  import SourceGeneratorCompilerPlugin._

  val name = SourceGeneratorCompilerPlugin.name
  val components = List(GenerationComponent)
  val description = "Generates optimized scala code"

  object GenerationComponent extends PluginComponent {
    val global = plugin.global
    val phaseName = SourceGeneratorCompilerPlugin.phase
    val runsAfter = List("namer")
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
          def applyFirstMatchingGenerator(gen:List[CodeStringReplacer]) {
            gen match {
              case Nil =>
              case head::tail =>
                val changed = head.replace(global)(tree, modified)
                if (!changed) applyFirstMatchingGenerator(tail)
            }
          }

          tree match {
            case PackageDef(ref, _) =>
              val packageName = sourceText.substring(ref.pos.start, ref.pos.end)
              modified.replace(ref.pos.start, ref.pos.end, packageName + ".compiled")
              modified.insert(ref.pos.end, s"\n\nimport ${classOf[Compiled].getName}")

            case md: ModuleDef =>
              modified.insert(md.pos.start, compiledShortTag(md.symbol.fullName('.')) + " ")

            case dd: DefDef if dd.pos.isRange =>
              //modified.replace(dd.pos.start, dd.pos.end, s"""${compiledShortTag(dd.symbol.fullName('.'))} def ${dd.name.toString}() = ${dd.rhs.toString()} """)
              modified.replace(dd.pos.start, dd.pos.end, s"""${compiledShortTag(dd.symbol.fullName('.'))} ${dd.toString()} """)
              //rockt: we still need to be able to pattern match on DefDefs within generators
              applyFirstMatchingGenerator(replacers)
            case other =>
              applyFirstMatchingGenerator(replacers)
          }
        }

        println(modified.current())
        val modifiedDir = new File(targetDir, "scalapplcodefest/sbt/compiled")
        println(modifiedDir.mkdirs())
        val modifiedFile = new File(modifiedDir, sourceFile.getName)
        val out = new PrintWriter(modifiedFile)
        out.println(modified.current())
        out.close()
      }
    }

  }

}

trait CodeStringReplacer {
  def replace(global: Global)(tree: global.Tree, modification:ModifiedSourceText): Boolean
}

class Compiled(original: String) extends StaticAnnotation


/**
 * Maintains a mapping from original character offsets to offsets in a modified string.
 * @param original the string to modify.
 */
class ModifiedSourceText(original: String, offset:Int = 0) {
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

  def fromTree(tree:Global#Tree) = {

  }

}


