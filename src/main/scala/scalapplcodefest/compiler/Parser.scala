package scalapplcodefest.compiler

import scala.util.parsing.combinator.JavaTokenParsers

/**
 *
 *
 * @author svivek
 */
object Parser extends JavaTokenParsers {

  override def skipWhitespace = true

  def apply(input: String): List[Statement] = parseAll(program, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.toString)
  }

  /**
   * Everything is a program
   * @return A parser for a list of statements
   */
  def program: Parser[List[Statement]] = rep(statement)

  /**
   * A statement can be an alias or a function definition or a parameter specification or comments
   * @return A parser that looks for one of the types of the statements
   */
  def statement = alias | functionDefinition | param | comments

  /**
   * Create a named alias for an expression
   * @return A parser for an alias statement
   */
  def alias = ident ~ "=" ~ expr ^^ {
    case id ~ _ ~ e => Alias(id, e)
  }

  def functionDefinition = "fun" ~ ident ~ "=" ~ expr ^^ {
    case _ ~ id ~ _ ~ functionDef => FunctionDefinition(id, functionDef)
  } |
    "fun" ~ ident ~ "(" ~ functionParams ~ ")" ~ "=" ~ expr ^^ {
      case _ ~ id ~ _ ~ params ~ _ ~ _ ~ functionDef => FunctionDefinition(id, functionDef, params)
    }

  def param = "param" ~ ident ~ ":" ~ typeExpression ^^ {
    case _ ~ id ~ _ ~ typeExpr => Params(id, typeExpr)
  }

  def comments = "//.*\n".r ^^ Comment

  def expr = ident ^^ Identifier | ident ^^ Identifier | literal

  def typeExpression_T: Parser[TypeExpression] =
    cartesianProduct |
      "set" ~ "(" ~ elements ~ ")" ^^ {case _ ~ _ ~ elems ~ _ => SetExpression(elems)} |
      ident ^^ {case s => TypeIdentifier(s)} |
      "(" ~ typeExpression ~ ")" ^^ {case _ ~ t ~ _ => t}


  def typeExpression: Parser[TypeExpression] =
    typeExpression_T ~ "->" ~ typeExpression ^^ {case l ~ _ ~ r => FunctionType(l, r)} |
      typeExpression_T

  def cartesianProduct: Parser[CartesianProductTypeExpression] =
    "cross" ~ "(" ~ typeExpressions ~ ")" ^^ {case _ ~ _ ~ te ~ _ => CartesianProductTypeExpression(te)}

  def typeExpressions: Parser[List[TypeExpression]] = repsep(typeExpression, ",")

  def elements: Parser[List[Literal]] = rep1sep(literal, ",")

  def functionParams = rep1sep(functionParam, ",")

  def functionParam = ident ~ ":" ~ typeExpression ^^ {case id ~ _ ~ typeExpr => Params(id, typeExpr)} |
    ident ^^ {id => Params(id)}

  def literal: Parser[Literal] =
    wholeNumber ^^ {case s => Integer(s.toInt)} |
      stringLiteral ^^ StringLiteral |
      decimalNumber ^^ {case s => RealNumber(s.toDouble)} |
      floatingPointNumber ^^ {case s => RealNumber(s.toDouble)} |
      "'" ~ ident ^^ {case _ ~ s => SymbolLiteral(s)}
}


sealed trait Statement

case class Comment(comment: String) extends Statement

case class Alias(id: String, expression: Expression) extends Statement

case class FunctionDefinition(name: String, body: Expression, params: List[Params] = Nil) extends Statement

case class Params(name: String, paramType: TypeExpression = TypeNotSpecified) extends Statement


trait Expression

case class Identifier(name: String) extends Expression

trait Literal extends Expression

case class StringLiteral(string: String) extends Literal

case class RealNumber(value: Double) extends Literal

case class Integer(value: Int) extends Literal

case class SymbolLiteral(name: String) extends Literal

trait TypeExpression extends Expression

object TypeNotSpecified extends TypeExpression

case class SetExpression(elements: List[Literal]) extends TypeExpression

case class TypeIdentifier(name: String) extends TypeExpression

case class FunctionType(left: TypeExpression, right: TypeExpression) extends TypeExpression

case class CartesianProductTypeExpression(sets: List[TypeExpression]) extends TypeExpression
