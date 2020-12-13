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


}
