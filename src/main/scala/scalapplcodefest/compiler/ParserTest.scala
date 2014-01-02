package scalapplcodefest.compiler

import scala.util.parsing.combinator.JavaTokenParsers

/**
 *
 *
 * @author svivek
 */
object ParserTest extends App {

  val program =
    """//persons = set("Anna", "Bob")
      S = persons -> bools
      C = persons -> bools
      F = cross(persons, persons) -> bools
      param smokes : S
      param cancer : C
      param friends : F
      param weights : vectors

//      fun pred(p:persons, smokes, cancer) = e('smokingIsBad)*I(smokes(p) |=> cancer(p))
//      fun f1 = sum (p: persons) => e('smokingIsBad)*I(smokes(p) |=> cancer(p))
//      fun f1p = sum (p: persons) => pred(p, _, _)

//      fun f2(smokes, friends) = {
//        sum (p1: persons, p2: persons) => e('peerPressure)*I(friends(p1, p2) |=> (smokes(p1) <=> smokes(p2)))
//      }

//      fun model = (f1 + f2) dot weights

//      fun model2(smokes, cancer, friends, weights) = (f1(_, _) + f2(smokes, _)) dot weights

//      fun conditioned = model | weights <- (0.1, 0.3)

//      best = argmax conditioned @bruteForceMax
    """.stripMargin

//  println(Parser(program))

  println(Parser.phrase(Parser.program)(new scala.util.parsing.input.CharArrayReader(program.toCharArray)))
//  val program =
//    """smokes = 1""".stripMargin
//
//  println(SandboxParser(program))
}
