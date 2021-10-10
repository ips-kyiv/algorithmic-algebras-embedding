package ua.ips.tools.schema2c

import java.nio.file.{Files,Paths}
import java.io.File;
import java.lang.ClassLoader
import java.net.URL
import java.net.URLClassLoader

import ua.ips.algo.*
import ua.ips.algo.translation.*
import ua.ips.algo.translation.languages.clike.*



class SchemaToC(config: Schema2CConfig) extends Translator(SchemaToC.buildTarget(config)) {



}

object SchemaToC {

  def main(args: Array[String]): Unit  = {
     val config = parseConfig(args)
     val transpiler = new SchemaToC(config)
     processAll(transpiler,config)
  }

  def processAll(transpiler: Translator,  config: Schema2CConfig): Unit = {
      val classLoader = config.inputJar match
        case Some(jar) => createCustomClassloader(jar)
        case None => this.getClass().getClassLoader()
      val name = config.inputObject          
  }

  def processObject(name: String, loader: ClassLoader,  transpiler: Translator, config: Schema2CConfig) = {
    val objClass = loader.loadClass(name+"$");
    val moduleField = objClass.getField("MODULE$")
    val obj = moduleField.get(null).asInstanceOf[SchemaProvider]
    val schema = obj.defaultSchema
    val nameComponents = name.split(".")
    val packageName =  if (nameComponents.length > 1) then {
       nameComponents.slice(0, nameComponents.length - 1).toIndexedSeq
    } else Seq()
    val schemaName = nameComponents(nameComponents.length - 1);
    val module = SchemaModule(packageName, schemaName, schema)
    val outputBundle = transpiler.compile(module)
    transpiler.target.language.write(outputBundle, config.outputDir)
  }

  def parseConfig(args: Array[String]): Schema2CConfig = {
      // TODO: parse args and modifu default.
      val config = Schema2CConfig()
      return config;
  }

  def buildTarget(config: Schema2CConfig): Target = {
     Target(
        language = PlainC,
        limits = config.target.limits,
        costs = config.target.costs
      )
  }

  def createCustomClassloader(jar: String): ClassLoader = {
    if (!Files.exists(Paths.get(jar))) then
      throw new IllegalArgumentException(s"file $jar not exists")
    val jarFile = new File(jar);
    val urls = Array[URL](jarFile.toURI().toURL());
    new URLClassLoader(urls,ClassLoader.getSystemClassLoader())
        

  }

}
