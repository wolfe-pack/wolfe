package scalapplcodefest.mln

import _root_.scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers}

//import com.google.common.base.Strings


/**
 * Markov Logic Network parser.
 * The syntax and expression rules for Alchemy MLN can be found here:
 * @see http://alchemy.cs.washington.edu/user-manual/3_1Input_Files.html
 *      and http://alchemy.cs.washington.edu/user-manual/4_2MLN_Syntax.html
 *
 */
object MLNParser extends JavaTokenParsers with RegexParsers {
  val LowerCaseID = """[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r
  val UpperCaseID = """[A-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r
  val NumDouble = "-?\\d+(\\.\\d+)?".r
  val NumPosInt = "\\d+".r
  val StringLit = "(\\w)*".r
  val multiline = "(/\\*(?:.|[\\n\\r])*?\\*/)"

  val minPrec = 1
  val maxPrec = 3

  override val whiteSpace = """(\s|//.+\n|(/\*(?:.|[\n\r])*?\*/))+""".r

  def mln: Parser[List[Expression]] = rep(expression)

  def expression: Parser[Expression] =
    (typeDefinitions ||| function ||| hardFormula ||| weightedFormula ||| asteriskFormula ||| formula ||| include)


  def typeDefinitions: Parser[Expression] = (integerTypeDefinition ||| constantTypeDefinition)

  /**
   * Types and constants can be declared in an .mln file with the following syntax:
   * <typename> = { <constant1>, <constant2>, ... },
   * e.g., person = { Alice, Bob }.
   * integer types, e.g., ageOfStudent = { 18, ..., 22 }.
   */
  def constantTypeDefinition: Parser[ConstantTypeDefinition] =
    (LowerCaseID ~ "=" ~ "{" ~ repsep(UpperCaseID, ",") ~ "}") ^^ {
      case name ~ "=" ~ "{" ~ constants ~ "}" => ConstantTypeDefinition(name, constants)
    }

  def integerTypeDefinition: Parser[IntegerTypeDefinition] =
    (LowerCaseID ~ "=" ~ "{" ~ NumPosInt ~ "," ~ "..." ~ "," ~ NumPosInt ~ "}") ^^ {
      case name ~ "=" ~ "{" ~ from ~ "," ~ "..." ~ "," ~ to ~ "}" => IntegerTypeDefinition(name, from.toInt, to.toInt)
    }


  def include: Parser[Include] = ("#include" ~> stringLiteral) ^^ {
    s => Include(s)
  }


  def formula: Parser[Formula] = binary(minPrec) ||| atomic ||| negatedFormula

  def atomic: Parser[Formula] = parenthesized ||| negatedAtom ||| plusAtom ||| atom ||| internalPredicateAtom

  def parenthesized: Parser[Formula] = "(" ~> formula <~ ")"


  def atom: Parser[Atom] = UpperCaseID ~ "(" ~ termList ~ ")" ^^ {
    case s ~ "(" ~ terms ~ ")" => Atom(s, terms)
  }

  def plusAtom: Parser[PlusAtom] = UpperCaseID ~ "(" ~ plusTermList ~ ")" ^^ {
    case s ~ "(" ~ terms ~ ")" => PlusAtom(s, terms)
  }

  def internalPredicateAtom: Parser[InternalPredicateAtom] = comparisonPred ||| substr

  def internalFunctions: Parser[InternalFunction] = internalFunc ||| succ ||| concat

  def comparisonPred: Parser[InternalPredicateAtom] = (LowerCaseID | NumDouble | NumPosInt) ~ (">" | "<" | ">=" | "<=" | "=" | "!=") ~ (LowerCaseID | NumDouble | NumPosInt) ^^ {
    case a1 ~ p ~ a2 =>
      InternalPredicateAtom(p, List(determineTermType(a1), determineTermType(a2)))
  }

  def substr: Parser[InternalPredicateAtom] = "substr" ~ "(" ~ (LowerCaseID | StringLit) ~ "," ~ (LowerCaseID | StringLit) ~ ")" ^^ {
    case _ ~ _ ~ s1 ~ _ ~ s2 ~ _ => InternalPredicateAtom("substr", List(determineTermType(s1), determineTermType(s2)))
  }

  def internalFunc: Parser[InternalFunction] = (LowerCaseID | NumPosInt) ~ ("+" | "-" | "*" | "/" | "%") ~ (LowerCaseID | NumPosInt) ^^ {
    case a1 ~ o ~ a2 => InternalFunction(VariableOrType("int"), o, List(determineTermType(a1), determineTermType(a2)))
  }

  def succ: Parser[InternalFunction] = "succ" ~ "(" ~ (LowerCaseID | NumPosInt) ~ ")" ^^ {
    case _ ~ _ ~ a ~ _ => InternalFunction(VariableOrType("int"), "succ", List(determineTermType(a)))
  }

  def concat: Parser[InternalFunction] = "concat" ~ "(" ~ (LowerCaseID | StringLit) ~ "," ~ (LowerCaseID | StringLit) ~ ")" ^^ {
    case _ ~ _ ~ s1 ~ _ ~ s2 ~ _ => InternalFunction(VariableOrType("string"), "concat", List(determineTermType(s1), determineTermType(s2)))
  }

  private def determineTermType(arg: String): MLNParser.Term = {
    if (arg.matches(LowerCaseID.toString)) VariableOrType(arg) else Constant(arg.toString)
  }


  def asteriskAtom: Parser[AsteriskAtom] = "*" ~ atom ^^ {
    case _ ~ a => AsteriskAtom(a.predicate, a.args)
  }

  def asteriskFormula: Parser[AsteriskFormula] = (binaryAsterisk(minPrec) ||| asteriskAtomic) ^^ {
    case f => AsteriskFormula(f)
  }

  def asteriskAtomic: Parser[Formula] = asteriskAtom ||| atom

  def binaryAsterisk(level: Int): Parser[Formula] = {
    if (level > maxPrec) asteriskAtomic
    else binaryAsterisk(level + 1) * binaryOp(level)
  }

  def negatedFormula: Parser[Not] = "!" ~ (parenthesized) ^^ {
    case _ ~ f => Not(f)
  }

  def negatedAtom: Parser[Not] = "!" ~ (parenthesized | atom | plusAtom) ^^ {
    case _ ~ a => Not(a)
  }

  def weightedFormula: Parser[WeightedFormula] = (NumDouble ~ formula) ^^ {
    case weight ~ formula => WeightedFormula(weight.toDouble, formula)
  }

  def hardFormula: Parser[HardFormula] = formula <~ "." ^^ {
    f => HardFormula(f)
  }

  def termList: Parser[List[Term]] = repsep(term, ",") ^^ {
    case t => t
  }

  def plusTermList: Parser[List[Term]] = repsep(plusTerm, ",") ^^ {
    case t => t
  }

  def typeList: Parser[List[VariableOrType]] = repsep(variable, ",") ^^ {
    case t => t
  }

  //  <returntype> <functionname>(<type1>, ... , <typen>)
  def function: Parser[FunctionDefinition] =
    LowerCaseID ~ StringLit ~ "(" ~ typeList ~ ")" ^^ {
      case retType ~ fName ~ "(" ~ t ~ ")" => FunctionDefinition(VariableOrType(retType), fName, t)
    }

  def groundedTermList: Parser[List[Term]] = repsep(groundedTerm, ",") ^^ {
    case t => t
  }

  def term: Parser[Term] = variable ||| constant ||| exclType ||| internalFunctions

  def plusTerm: Parser[Term] = plusVariable ||| variable ||| constant ||| exclType

  def groundedTerm: Parser[Term] = (constant)

  def variable: Parser[VariableOrType] = LowerCaseID ^^ {
    s => VariableOrType(s)
  }

  def constant: Parser[Constant] = UpperCaseID ^^ {
    s => Constant(s)
  }

  def exclType: Parser[ExclamationVariable] = LowerCaseID <~ "!" ^^ {
    s => ExclamationVariable(s)
  }

  def plusVariable: Parser[PlusVariable] = "+" ~> LowerCaseID ^^ {
    s => PlusVariable(s)
  }

  def deterministic: Parser[WeightedFormula] = formula <~ "." ^^ {
    f => WeightedFormula(Double.PositiveInfinity, f)
  }

  /*Operator precedence is as follows:
  not > and > or > implies > if and only if > forall = exists.*/
  def binaryOp(level: Int): Parser[((Formula, Formula) => Formula)] = {
    level match {
      case 1 =>
        "v" ^^^ {
          (a: Formula, b: Formula) => Or(a, b)
        }
      case 2 =>
        "=>" ^^^ {
          (a: Formula, b: Formula) => Implies(a, b)
        } |
          "<=>" ^^^ {
            (a: Formula, b: Formula) => Equivalence(a, b)
          }
      case 3 =>
        "^" ^^^ {
          (a: Formula, b: Formula) => And(a, b)
        }
      case _ => throw new RuntimeException("bad precedence level " + level)
    }
  }


  def binary(level: Int): Parser[Formula] = {
    if (level > maxPrec) atomic
    else binary(level + 1) * binaryOp(level)
  }


  /**
   * parsing the database files
   *
   */

  def db: Parser[Any] = (databaseFunction ||| databaseAtom)

  val positive = true

  def positiveDatabaseAtom: Parser[DatabaseAtom] = UpperCaseID ~ "(" ~ groundedTermList ~ ")" ^^ {
    case s ~ "(" ~ terms ~ ")" => DatabaseAtom(s, terms, positive)
  }

  def negativeDatabaseAtom: Parser[DatabaseAtom] = "!" ~ UpperCaseID ~ "(" ~ groundedTermList ~ ")" ^^ {
    case _ ~ s ~ "(" ~ terms ~ ")" => DatabaseAtom(s, terms, !positive)
  }

  def databaseAtom: Parser[DatabaseAtom] = (positiveDatabaseAtom | negativeDatabaseAtom) ^^ {
    t => t
  }

  def dbFunctions: Parser[List[DatabaseFunction]] = rep(databaseFunction) ^^ {
    case t => t
  }

  def databaseFunction: Parser[DatabaseFunction] = constant ~ "=" ~ StringLit ~ "(" ~ groundedTermList ~ ")" ^^ {
    case value ~ "=" ~ fName ~ "(" ~ cons ~ ")" => DatabaseFunction(value, fName, cons)
  }

  trait Expression

  case class IntegerTypeDefinition(name: String, from: Int, to: Int) extends Expression

  case class ConstantTypeDefinition(name: String, constants: Seq[String]) extends Expression

  case class Include(fileName: String) extends Expression

  case class FunctionDefinition(returnType: VariableOrType, name: String, types: List[VariableOrType]) extends Expression

  case class InternalFunction(returnType: VariableOrType, name: String, types: List[Term]) extends Term

  trait Term extends Expression {
    def subterms: Seq[Term] = Seq()

    lazy val allVariables: Set[Variable] = this match {
      case v: Variable => Set(v)
      case _ => subterms.foldLeft(Set[Variable]()) {
        (r, s) => r ++ s.allVariables
      }
    }
    lazy val allPlusVariables: Seq[PlusVariable] =
      allVariables.filter(_.isInstanceOf[PlusVariable]).map(_.asInstanceOf[PlusVariable]).toSeq
  }

  case class Constant(value: String) extends Term

  trait Variable extends Term {
    def name: String
  }

  case class VariableOrType(name: String) extends Variable {
    override def toString: String = name
  }

  case class ExclamationVariable(name: String) extends Variable {
    override def toString: String = name
  }

  case class PlusVariable(name: String) extends Variable {
    override def toString: String = name
  }

  sealed trait Formula extends Expression {
    def subformulas: Seq[Formula] = Seq()

    lazy val allVariables: Set[Variable] = this match {
      case Atom(_, args) => args.foldLeft(Set[Variable]())(_ ++ _.allVariables)
      case PlusAtom(_, args) => args.foldLeft(Set[Variable]())(_ ++ _.allVariables)
      case _ => this.subformulas.foldLeft(Set[Variable]()) {
        _ ++ _.allVariables
      }
    }
    lazy val allPlusVariables =
      allVariables.filter(_.isInstanceOf[PlusVariable]).map(_.asInstanceOf[PlusVariable]).toSeq

  }

  case class WeightedFormula(weight: Double, formula: Formula) extends Formula {
    override def subformulas = Seq(formula)
  }

  case class HardFormula(formula: Formula) extends Formula {
    override def subformulas: Seq[Formula] = Seq(formula)
  }

  case class AsteriskFormula(formula: Formula) extends Formula {
    override def subformulas: Seq[Formula] = Seq(formula)
  }


  case class Atom(predicate: String, args: List[Term]) extends Formula

  case class InternalPredicateAtom(predicate: String, args: List[Term]) extends Formula

  case class NegatedAtom(predicate: String, args: List[Term]) extends Formula

  case class AsteriskAtom(predicate: String, args: List[Term]) extends Formula

  case class PlusAtom(predicate: String, args: List[Term]) extends Formula

  case class DatabaseAtom(predicate: String, args: List[Term], positive: Boolean)

  case class DatabaseFunction(returnValue: Term, name: String, values: List[Term])

  case class Not(arg: Formula) extends Formula {
    override def subformulas = Seq(arg)
  }

  case class And(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)
  }

  case class Or(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)
  }

  case class Implies(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)
  }

  case class Equivalence(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)
  }


}



