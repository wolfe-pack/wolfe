import java.io.File

import ml.wolfe.ui.HTMLFileRenderer

val renderer = new HTMLFileRenderer(new File("/tmp/example.html"))
import renderer._

md("#Example Header 2")

md("""
Some text.

## Another header

""")

close()