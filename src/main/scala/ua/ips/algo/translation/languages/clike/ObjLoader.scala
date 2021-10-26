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
class ObjLoader(target: Target) extends Loader(target) {

    import scala.concurrent.ExecutionContext.Implicits.global

    // library should be loaded only onve, so we keep the list of loaded names and directories here.
    private val loadedNames = new ConcurrentHashMap[String,Interpretation]()
 
    def prepare(signature: DataSortSignature, path: String, variant: Seq[String]): Future[Unit] = 
      ccompile(signature, path, variant)

    def ccompile(signature: DataSortSignature, path: String, variant: Seq[String]): Future[Unit] = async[Future] {
       val pbCMake = new ProcessBuilder("cmake", path)
       val cmakeProcess = pbCMake.start()
       val cmakeExit = cmakeProcess.onExit()
       // TODO: output cmake output, maybe via channel or asynclist
       await(cmakeExit) 
       if (cmakeProcess.exitValue() != 0) {
         throw new IllegalStateException("cmake return non-zero exit code");
       }
       val pbMake = new ProcessBuilder("make").directory(new File(path))
       val makeProcess = pbMake.start()
       // TODO: outpt pbMake output.
       val makeExit = makeProcess.onExit()
       // TODO: output make output
       if (makeProcess.exitValue() != 0) {
         throw new IllegalStateException("make return non-zero exit code");
       }
       // val libfunname = mangleSignature(signature)
       // system process
       // TODO: move lib to libdir ?
    }

    
    def load(signature: DataSortSignature, path: String, variant: Seq[String]): Interpretation = {

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
      System.loadLibrary(moduleName);
      //
      val cLinker = CLinker.getInstance()

      val functionName = NamesMangling.objFunctionName(signature,variant)
      val optMainFun = CLinker.systemLookup().lookup(functionName)
      if (optMainFun.isEmpty) {
        // TODO: full diagnostics.
        throw new TranslationException(s"object for name ${functionName} is not found in ${moduleName}")
      } 
      val interpretation = new Jep412Caller(signature, optMainFun.get, variant)
      loadedNames.put(moduleName, interpretation)
      return interpretation;
    }

    
 
}