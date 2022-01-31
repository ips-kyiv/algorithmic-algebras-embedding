package ua.ips.algo.translation.languages.clike



// TODO: set jdk-option to enable 
import jdk.incubator.foreign.*
import jdk.incubator.foreign.CLinker.VaList.Builder
import java.lang.invoke.*

import ua.ips.algo.*
import ua.ips.algo.translation.*


class Jep412Interpretation(mainSignature: DataSortSignature, mainFunctionAddress: MemoryAddress, variant: Seq[String]) extends LoadedInterpretation {

   type DataItem = MemoryAddress | Boolean | Int |  Long | Float | Double

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
      rep.dataSort match
         case BasicDataSort(name) =>
            name match
               case BooleanBasicRep.name => 
                  item match
                     case x: Boolean => Some(x.asInstanceOf[T])
                     case _ => throw new InterpretationException("boolean was expexted");
               case IntBasicRep.name => 
                  item match
                     case x: Int => Some(x.asInstanceOf[T])
                     case _ => throw new InterpretationException("int was expexted");
               case LongBasicRep.name => 
                  item match
                     case x: Long => Some(x.asInstanceOf[T])
                     case _ => throw new InterpretationException("long was expexted");                     
               case FloatBasicRep.name => CLinker.C_FLOAT
                  item match
                     case x: Float => Some(x.asInstanceOf[T])
                     case _ => throw new InterpretationException("float was expexted");
               case DoubleBasicRep.name => 
                  item match
                     case x: Double => Some(x.asInstanceOf[T])
                     case _ => throw new InterpretationException("double was expexted");
               case UnitBasicRep.name => Some(().asInstanceOf[T])
               case _ =>
                  throw InterpretationException(s"Unknown primitive type for dataSort: $name");
         case TupleDataSort(elements)=> ???
         case NamedSet(_) => ???
         case TensorDataSort(_,_) => ???  
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


   def javaRefConstant(scope: ResourceScope, value: AnyRef, rep: DataSortRep[?]): DataItem = {
      rep.dataSort match
         case BasicDataSort(name) =>
            name match
               case BooleanBasicRep.name => 
                  value match
                     case x: java.lang.Boolean => x.booleanValue
                     case _ => throw new InterpretationException(s"java.lang.Boolean was expected, we have $value");
               case IntBasicRep.name => 
                  value match
                     case x: java.lang.Integer => x.intValue
                     case _ => throw new InterpretationException(s"java.lang.Integer was expexted, we have $value");
               case LongBasicRep.name => 
                  value match
                     case x: java.lang.Long => x.longValue
                     case _ => throw new InterpretationException(s"java.lang.Long was expected, we have $value");                     
               case FloatBasicRep.name => CLinker.C_FLOAT
                  value match
                     case x: java.lang.Float => x.floatValue
                     case _ => throw new InterpretationException(s"java.lang.Float was expected, we have $value");
               case DoubleBasicRep.name => 
                  value match
                     case x: java.lang.Double => x.doubleValue
                     case _ => throw new InterpretationException(s"java.lang.Double was expected, we have $value");
               case UnitBasicRep.name => 0
               case _ =>
                  throw InterpretationException(s"Unknown primitive type for dataSort: $name");
         case TupleDataSort(elements)=> ???
         case NamedSet(_) => ???
         case TensorDataSort(_,_) => ???  
   
   }

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
      sort match
         case BasicDataSort(name) =>
            name match
               case BooleanBasicRep.name => CLinker.C_INT
               case IntBasicRep.name => CLinker.C_INT
               case LongBasicRep.name => CLinker.C_LONG
               case FloatBasicRep.name => CLinker.C_FLOAT
               case DoubleBasicRep.name => CLinker.C_DOUBLE
               case UnitBasicRep.name => 
                  throw InterpretationException("Can't get memory layput for unit");
               case _ =>
                  throw InterpretationException(s"Unknown primitive type for dataSort: $name");
         case TupleDataSort(elements)=> CLinker.C_POINTER
         case NamedSet(_) => CLinker.C_POINTER
         case TensorDataSort(_,_) => CLinker.C_POINTER  
   }

  

}



