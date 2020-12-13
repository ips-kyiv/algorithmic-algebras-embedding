package embedded


import ua.ips.algo._
import ua.ips.algo.given

import munit._

class ParseMinimalSchemaTest extends FunSuite {


  test("parseMinimalSchema") {

    val schema = Schema.build{
      (x:Int) => x+1
    }
    println(schema)
    assert(true)

  }

  test("parseCondition") {

    val schema = Schema.build{
      (x:Int) => if (x > 0) 1 else 0
    }
    println(schema)
    schema match
      case SequentialSchema(
              InputSchema(variable,sort),
              ConditionalSchema(c,x,y)
           ) =>
         assert(true)
      case _ =>
         assert(false)


  }



}
