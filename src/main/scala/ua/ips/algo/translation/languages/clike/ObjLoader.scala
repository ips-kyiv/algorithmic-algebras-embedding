package ua.ips.algo.translation.languages.clike

import ua.ips.algo.*
import ua.ips.algo.translation.*

import scala.concurrent.*
import java.io.File
import java.util.concurrent.ConcurrentHashMap

import cps.*
import cps.monads.{given,*}

import jdk.incubator.foreign.*


/**
 * load objec file for schema.
 **/
trait ObjLoader extends Loader {

    import scala.concurrent.ExecutionContext.Implicits.global

    // library should be loaded only onve, so we keep the list of loaded names and directories here.
    private val loadedNames = new ConcurrentHashMap[String,LoadedInterpretation]()
 
    def prepare(signature: DataSortSignature, path: String, variant: Seq[String]): Future[LoadedInterpretation] = 
      ccompile(signature, path, variant)

    def ccompile(signature: DataSortSignature, path: String, variant: Seq[String]): Future[LoadedInterpretation] = async[Future] {
       val fPath = new File(path)
       val pbCMake = new java.lang.ProcessBuilder("cmake", ".").inheritIO().directory(fPath)
       println("starting cmake");
       //val logger = ProcessLogger(s => println(s))
       val cmakeProcess = pbCMake.start()
       
       //cmakeProcess.waitFor()
       val cmakeExit = cmakeProcess.onExit()
       // TODO: output cmake output, maybe via channel or asynclist
       await(cmakeExit) 
       if (cmakeProcess.exitValue() != 0) {
         throw new IllegalStateException("cmake return non-zero exit code");
       }
       val pbMake = new ProcessBuilder("make").inheritIO().directory(fPath)
       val makeProcess = pbMake.start()
       // TODO: outpt pbMake output.
       val makeExit = makeProcess.onExit()
       await(makeExit)
       // TODO: output make output
       if (makeProcess.exitValue() != 0) {
         throw new IllegalStateException("make return non-zero exit code");
       }

       load(signature, path, variant)

    }

    
    def load(signature: DataSortSignature, path: String, variant: Seq[String]): LoadedInterpretation = {

      val moduleName = NamesMangling.objModuleName(signature, variant)
      val name = s"${path}/${moduleName}";
      var retval = loadedNames.get(name, variant) 
      if !(retval eq null ) then {
        // library should be loaded only onve, so we keep the list of loaded names
        return retval
      }
      // 
      // TODO:  add path to set of laoded classes. 
      //  Now, think that out dir is in classpath
      println(s"loading library ${path}/${moduleName}")
      val fullPath = java.nio.file.Paths.get(s"${path}/${moduleName}")
      System.load(fullPath.toAbsolutePath.toString);
      //
      val cLinker = CLinker.getInstance()
      val functionName = NamesMangling.objFunctionName(signature,variant)
      val optMainFun = SymbolLookup.loaderLookup().lookup(functionName)
      if (optMainFun.isEmpty) {
        // TODO: full diagnostics.
        println(s"object for name ${functionName} is not found in ${moduleName}")
        throw new TranslationException(s"object for name ${functionName} is not found in ${moduleName}")
      } 

      val interpretation = new Jep412Interpretation(signature, optMainFun.get, variant)
      loadedNames.put(moduleName, interpretation)
      return interpretation;
    }

    
 
}