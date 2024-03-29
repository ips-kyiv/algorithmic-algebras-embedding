package compileAndRun

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import ua.ips.algo.{*, given}
import ua.ips.tools.schema2c.*

import cps.*
import cps.monads.{*,given}

import java.io.File


import munit.*

class MinimalSchemaCompileTest extends FunSuite:

  test("parse compile and run minimal schema") {

    val schema = Schema.build{
      (x:Int) => x+1
    }
    println(schema)
    

    val cfg = Schema2CConfig()
    val translator = SchemaToC(cfg)
    val schemaModule = SchemaModule(Seq(),"testmin", schema)
    val signature = schemaModule.extractSignature()

    //val interpreter = translator.translateAndLoad(schemaModule)

    val outputBundle = translator.compile(schemaModule)
    val outputDir = "testdata/compileAndRunMinimalSchema"
    val outputDirFile = new File(outputDir)
    if (! outputDirFile.exists()) {
      outputDirFile.mkdirs()
    }
    translator.target.language.write(outputBundle, outputDir)
    val fut = async[Future] {
      val interpreter = await(translator.prepare(signature, outputDir, Seq()))
      val x = interpreter.apply(signature.typesOnly, Seq(5))
      println(s"received value $x")
      assert(x.value == 6)
    }
    fut  // munit will care
    //Await.result(fut, 1.minute)
  }

