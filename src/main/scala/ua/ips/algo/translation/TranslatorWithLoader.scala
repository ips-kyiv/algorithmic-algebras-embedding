package ua.ips.algo.translation

import scala.concurrent.Future
import java.nio.file.Files
import ua.ips.algo.*


abstract class TranslatorWithLoader(override val target: Target) extends Translator(target) with Loader {


   def translateAndLoad(input: SchemaModule, variant: Seq[String], compileDir:Option[String]=None):Future[LoadedInterpretation] = {
      val outputBundle = compile(input)
      val tmpName = compileDir.getOrElse(input.name + "_" + variant.mkString("_"))
      val path = Files.createTempDirectory(tmpName).toString
      target.language.write(outputBundle, path)
      prepare(input.extractSignature(), path, variant)
   }


}