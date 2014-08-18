package ml.wolfe.examples

import ml.wolfe.Wolfe
import ml.wolfe.Wolfe._
import ml.wolfe.util.NLP._

import scala.util.matching.Regex

/**
 * @author luke
 */
object NERFeatures {

  val allFeatures = wordFeatures ++ boolFeatures ++ regexFeatures
  val miniFeatures = wordFeatures ++ boolFeaturesMini ++ regexFeaturesMini

  def apply(word:String, prefix:String = "", mini:Boolean = false) =
    (if(mini) miniFeatures else allFeatures).foldLeft( Wolfe.Vector() ){
      (acc, f) => acc + f(word, prefix)
    }

  // -----------------------------------


  def wordFeatures = Seq[(Symbol, String => String)](
    'word -> (t => t.toLowerCase),
    'prefix2 -> (t => t.take(2)),
    'suffix2 -> (t => t.takeRight(2))
  ).map({ case (sym:Symbol, fun:(String => String)) =>
    (word:String, prefix:String) => oneHot(prefix + sym -> fun(word))
  })


  // ------------ Mini Features -----------

  // Taken directly from https://github.com/wolfe-pack/wolfe/blob/052a72850221515b349a8b6a7cba2474637dde3d/wolfe-examples/src/main/scala/ml/wolfe/examples/NERExample.scala

  def regexFeaturesMini = Seq[(Symbol, Regex)](
    'allCap     -> "[A-Z]+".r,
    'realNumber -> "[-0-9]+[.,]+[0-9.,]+".r,
    'isDash     -> "[-–—−]".r,
    'isQuote    -> "[„“””‘’\"']".r,
    'isSlash    -> "[/\\\\]".r
  ).map({ case (sym:Symbol, reg:Regex) =>
    (word:String, prefix:String) => oneHot(prefix + sym, I(reg.pattern.matcher(word).matches))
  })

  def boolFeaturesMini = Seq[(Symbol,String => Boolean)] (
    'FirstCap   -> (t => t.head.isUpper)
  ).map({ case (sym:Symbol, fun:(String => Boolean)) =>
    (word:String, prefix:String) => oneHot(prefix + sym, I(fun(word)))
  })

  // ----------- Full Features -----------------------

  def regexFeatures = {
    val AminoAcidShortString = "Ala|Arg|Asn|Asp|Cys|Gln|Glu|Gly|His|Ile|Leu|Lys|Met|Phe|Pro|Ser|Thr|Trp|T.r,|Val|" +
    "Ter|Sec|Pyl|Asx|Glx|Xle|Xaa"

    Seq[(Symbol, Regex)](

      //taken from BANNER
      'Alpha -> "[A-Za-z]+".r,
      'InitCaps -> "[A-Z].*".r,
      'UpperLower -> "[A-Z][a-z].*".r,
      'LowerUpper -> "[a-z]+[A-Z]+.*".r,
      'AllCaps -> "[A-Z]+".r,
      'MixedCaps -> "[A-Z][a-z]+[A-Z][A-Za-z]*".r,
      'SingleChar -> "[A-Za-z]".r,
      'SingleDigit -> "[0-9]".r,
      'DoubleDigit -> "[0-9][0-9]".r,
      'Number -> "[0-9,]+".r,
      'HasDigit -> ".*[0-9].*".r,
      'Alphanumeric1 -> ".*[0-9].*[A-Za-z].*".r,
      'Alphanumeric2 -> ".*[A-Za-z].*[0-9].*".r,
      'NumbersLetters -> "[0-9]+[A-Za-z]+".r,
      'LettersNumber -> "[A-Za-z]+[0-9]+".r,

      'HasDash -> ".*-.*".r,
      'HasQuote -> ".*'.*".r,
      'HasSlash -> ".*/.*".r,
      'Realnumber1 -> "(-|\\+)?[0-9,]+(\\.[0-9]*)?%?".r,
      'Realnumber2 -> "(-|\\+)?[0-9,]*(\\.[0-9]+)?%?".r,
      'StartMinus -> "-.*".r,
      'StartPlus -> "\\+.*".r,
      'EndPercent -> ".*%".r,

      'Roman -> "[IVXDLCM]+".r,
      'Greek -> ("(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi.r,ho|sigma|tau|" +
      "upsilon|phi|chi|psi|omega)").r,
      'IsPunct -> "[`~!@#$%^&*()-=_+\\[\\]\\{}|;\':\",./<>?]+".r,

      //taken from ABNER
      'Year -> "19\\d\\d".r,
      'YearDecade -> "19\\d\\ds".r,
      'YearSpan -> "19\\d\\d-\\d+".r,
      'Fraction -> "\\d+\\\\/\\d".r,
      'DateLineDate1 -> "19\\d\\d-\\d\\d-\\d--d".r,
      'DateLineDate2 -> "19\\d\\d-\\d\\d-\\d\\d".r,

      //taken from http://hg-iesl.cs.umass.edu/hg/mallet/file/tip/src/cc/mallet/share/mccallum/ner/TUI.java
      'MultiDots -> "\\.\\.+".r,
      'EndsInDot -> "[^\\.]+.*\\.".r,
      'Acronym -> "[A-Z][A-Z\\.]*\\.[A-Z\\.]*".r,

      //taken from http://hg-iesl.cs.umass.edu/hg/mallet/file/def91eb6fb6f/src/cc/mallet/share/upenn/ner/NEPipes.java
      'mQuoted -> "([\"'].*[\"'])".r,
      'mBracketed -> "([({\\[].*[)}\\]])".r,
      'mInitial -> "([A-Z][.])".r,
      'mDots -> "([.][.])".r,
      'mDashes -> "(--)".r,
      'mFraction -> "(<DIGITS>/<DIGITS>)".r,

      //taken from JNET http://http://www.julielab.de/Resources/Software/NLP+Tools.html
      'InitDash -> "[-.*]".r,
      'EndDash -> "[.*-]".r,
      'Punctuation -> "[,.;:?!-+]".r,

      //taken from Klinger et al. (2008)
      'IsDash -> "[-–—−]".r,
      'IsSlash -> "[/\\\\]".r,
      'IsQuote -> "[„“””‘’\"']".r,

      //taken from Kuo et al. FIXME: not used yet
      'ThreeDigit -> "[0-9][0-9][0-9]".r,
      'FourDigit -> "[0-9][0-9][0-9][0-9]".r,
      'MoreDigit -> "[0-9][0-9][0-9][0-9][0-9]+".r,
      //val Nucleoside = "" //TODO, e.g., Thymine
      //val Nucleotide = "" //TODO, e.g., ATP

      'ATCGU -> "ATCGU".r,
      //val NucleicAcid = "" //TODO, e.g., cDNA
      //val AminoAcidLong = "" //TODO, e.g., tyrosine

      'AminoAcidShort -> AminoAcidShortString.r,

      'AminoAcidAndPosition -> (AminoAcidShortString + "[0-9]+").r, //TODO, e.g., Ser150
      'Vowel -> "a|e|i|o|u|A|E|I|O|U".r
    ).map({ case (sym:Symbol, reg:Regex) =>
      (word:String, prefix:String) => oneHot(prefix + sym, I(reg.pattern.matcher(word).matches))
    })
  }

  def boolFeatures = Seq[(Symbol,String => Boolean)] (
    'FirstCap   -> (t => t.head.isUpper),
    'EndCap     -> (t => t.last.isUpper),
    'SingleCap  -> (t => t.count(_.isUpper) == 1),
    'TwoCap     -> (t => t.count(_.isUpper) == 2 && t.size == 2),
    'ThreeCap   -> (t => t.count(_.isUpper) == 3 && t.size == 3),
    'MoreCap    -> (t => t.count(_.isUpper) > 3),
    Symbol("WORD_LENGTH=1") -> (t => t.size == 1),
    Symbol("WORD_LENGTH=2") -> (t => t.size == 2),
    Symbol("WORD_LENGTH=3-5") -> (t => t.size >= 3 && t.size <= 5),
    Symbol("WORD_LENGTH=6+") -> (t => t.size >= 6)
  ).map({ case (sym:Symbol, fun:(String => Boolean)) =>
    (word:String, prefix:String) => oneHot(prefix + sym, I(fun(word)))
  })
}
