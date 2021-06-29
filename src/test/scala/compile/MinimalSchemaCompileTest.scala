package compile

import ua.ips.algo.{*, given}
import ua.ips.tools.schema2c.*

import munit.*

class MinimalSchemaCompileTest extends FunSuite:

  test("parse and compile minimal schema") {

    val schema = Schema.build{
      (x:Int) => x+1
    }
    println(schema)
    val translator = SchemaToC(Schema2CConfig())
    val outputBundle = translator.compile(schema)
    translator.target.language.write(outputBundle, "testdata/compiletest1")
    assert(true)

  }



