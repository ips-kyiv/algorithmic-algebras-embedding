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
      
      val argsReprs: Seq[DataSortRep[?]] = signature.in.map(DataSort.findRep)
      val outRepr = DataSort.findRep(signature.out)
 
      val methodHandle: MethodHandle = CLinker.getInstance().downcallHandle(
         funAddress,
         createMethodType(),
         FunctionDescriptor.of(
            getMemoryLayout(signature.out),
            signature.in.map(x => getMemoryLayout(x)): _*
         )
      )

  
      def call(scope: DataScope, args: Seq[DataItem]): DataItem = {
         val jArgs = (args.zip(argsReprs)).map{ case (item, repr) =>
            extract(scope, item, repr).getOrElse{
               throw new InterpretationException(s"Can't find representation for data-sort ${item}")
            }
         }
         val retval = methodHandle.invokeWithArguments(jArgs: _*)
         javaRefConstant(scope, retval, outRepr)
      }

      def createMethodType(): MethodType = {
         MethodType.methodType(outRepr.javaClass, argsReprs.map(_.javaClass).toArray)
      }
   
   }

   val mainCaller = new Jep412Caller(mainSignature.typesOnly, mainFunctionAddress) 

   def newDataScope(): ResourceScope = ResourceScope.newConfinedScope()

   def extract[T](scope: ResourceScope, item: DataItem, rep: DataSortRep[T]): Option[T] = {
      ???
   }
 
   def constant[T](scope: ResourceScope, value: T, rep: DataSortRep[T]): DataItem = 
      rep.dataSort match
         case BasicDataSort(name) =>
            name match 
               case IntBasicRep.name => 
                  value match
                     case v: Int => v
                     case _ => throw InterpretationException(s"Value $value is not int")
               case _ => ???
         case _ => ???


   def javaRefConstant(scope: ResourceScope, value: AnyRef, rep: DataSortRep[?]): DataItem = ???

     // interpert signature or throw exception is one is not implemented
   def applyInternal(scope: ResourceScope, signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem = {
      if (signature == mainSignature.typesOnly) {
         mainCaller.call(scope, args)
      } else {
         throw InterpretationException(s"loaded and called signatures are different, loaded: ${mainSignature.typesOnly}, call:${signature}")
      }
   }

   override def  resolve(signature: TypesOnlyDataSortSignature): LoadedFunctionCaller =
      ???

 
   def getMemoryLayout(sort: DataSort): MemoryLayout = {
      ???
   }

}



