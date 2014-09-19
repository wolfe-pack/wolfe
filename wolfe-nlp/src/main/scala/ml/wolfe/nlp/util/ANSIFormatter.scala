package ml.wolfe.nlp.util

/**
 * Created by rockt on 19/09/2014.
 */

object ANSIFormatter {
  implicit class ANSIString(string: String) {

    import Console._

    private def apply(modifier: String) = modifier + string + RESET

    //foreground
    def black() = this(BLACK)
    def blue() = this(BLUE)
    def cyan() = this(CYAN)
    def green() = this(GREEN)
    def magenta() = this(MAGENTA)
    def red() = this(RED)
    def yellow() = this(YELLOW)
    def white() = this(WHITE)

    //background
    def onBlack() = this(BLACK_B)
    def onBlue() = this(BLUE_B)
    def onCyan() = this(CYAN_B)
    def onGreen() = this(GREEN_B)
    def onMagenta() = this(MAGENTA_B)
    def onRed() = this(RED_B)
    def onYellow() = this(YELLOW_B)
    def onWhite() = this(WHITE_B)

    //style
    def blink() = this(BLINK)
    def bold() = this(BOLD)
    def invisible() = this(INVISIBLE)
    def reversed() = this(REVERSED)
    def underlined() = this(UNDERLINED)
  }
}
