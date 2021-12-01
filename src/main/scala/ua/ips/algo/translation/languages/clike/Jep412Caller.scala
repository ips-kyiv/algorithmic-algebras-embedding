package ua.ips.algo.translation.languages.clike



// TODO: set jdk-option to enable 
import jdk.incubator.foreign.*
import jdk.incubator.foreign.CLinker.VaList.Builder
import java.lang.invoke.*

import ua.ips.algo.*
import ua.ips.algo.translation.*


class Jep412Interpretation(mainSignature: DataSortSignature, mainFunctionAddress: MemoryAddress, variant: Seq[String]) extends LoadedInterpretation {

   type DataItem = MemoryAddress | Int

   type DataScope = ResourceScope

   class Jep412Caller(override val signature: TypesOnlyDataSortSignature, funAddress: MemoryAddress) extends LoadedFunctionCaller {
      var methodHandle: MethodHandle = null

      def call(scope: DataScope, args: Seq[DataItem]): DataItem = ???

   }


   def newDataScope(): ResourceScope = ResourceScope.newConfinedScope()

   def extract[T](scope: ResourceScope, item: DataItem, rep: DataSortRep[T]): Option[T] = {
      ???
   }
 
   def constant[T](scope: ResourceScope, value: T, rep: DataSortRep[T]): DataItem = ???

     // interpert signature or throw exception is one is not implemented
   def applyInternal(scope: ResourceScope, signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem = {
      ???
   }

   override def  resolve(signature: TypesOnlyDataSortSignature): LoadedFunctionCaller =
      ???

}



