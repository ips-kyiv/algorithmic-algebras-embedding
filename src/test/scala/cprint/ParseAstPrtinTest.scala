package cprint

import ua.ips.algo._
import ua.ips.algo.given
import ua.ips.algo.translation._

import munit._

class ParseAstPrintTest extends FunSuite {

   test("print simple expression") {

    import ua.ips.algo.translation.languages.clike._ 
    import ua.ips.algo.translation.languages.clike.given
    

    val cAst = (
      ExpressionStatement(
       EqualityBinaryExpression(
        BinaryOperator.BINARY_PLUS, 
        Identifier("x"),
        IntConstant(1)
       )
      )
    )

    val printState0 = PrintState.create()
    val state = cAst.print(printState0)
    val res = state.printedString()
    println(res)

    assert(res.contains("x"))
    assert(res.contains("1"))
    assert(res.contains("+"))

   }


}