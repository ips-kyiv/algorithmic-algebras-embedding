package ua.ips.algo

import ua.ips.algo.runtime.*

import scala.compiletime.*
import scala.quoted.*
import scala.collection.ArrayOps


sealed trait DataSort:
  def lift(using Quotes): Expr[DataSort]

case class BasicDataSort(name:String) extends DataSort:
  def lift(using Quotes): Expr[DataSort] = '{ BasicDataSort(${Expr(name)}) }

case class TupleDataSort(elements: IndexedSeq[DataSort]) extends DataSort:
  def lift(using Quotes) = '{ TupleDataSort(${Expr.ofSeq(elements.map(_.lift))}.toIndexedSeq) }

case class NamedSet(elements: Map[String,DataSort]) extends DataSort:
  def lift(using Quotes): Expr[DataSort] =
       '{ NamedSet( ${Expr.ofSeq(
              elements.toSeq.map(x => Expr.ofTupleFromSeq(Seq(Expr(x._1),x._2.lift)).asExprOf[(String,DataSort)])
        )}.toMap ) }


enum TensorDataSortFlawor:
      case Dence, Diagonal, AllSame   

object TensorDataSortFlawor:
   //TODO: submit feature request to dotty      
   given ToExpr[TensorDataSortFlawor] with
      def apply(x: TensorDataSortFlawor)(using Quotes): Expr[TensorDataSortFlawor] =
         x match
            case TensorDataSortFlawor.Dence => '{ TensorDataSortFlawor.Dence }
            case TensorDataSortFlawor.Diagonal => '{ TensorDataSortFlawor.Diagonal }
            case TensorDataSortFlawor.AllSame => '{ TensorDataSortFlawor.AllSame }
         
         
     
case class TensorDataSort[E](element: DataSort, flawor: TensorDataSortFlawor) extends DataSort:
   def lift(using Quotes): Expr[DataSort] =
      '{ TensorDataSort( ${element.lift}, ${Expr(flawor)} ) }
     

object DataSort:

   def findRep(sort:DataSort): DataSortRep[?] =
      sort match
         case BasicDataSort(name) =>
            BasicDataSortRep.find(name)
         case TupleDataSort(elements) =>
            TupleDataSortRep.find(elements)
         case ns:NamedSet =>
            NamedSetRep.find(ns)
         case TensorDataSort(element, flawor) =>
            TensorDataSortRep.find(element, flawor)   
         



trait DataSortRep[T]:

   type Value = T
   
   def dataSort: DataSort

   def extractItem(interpretation:Interpretation)(scope: interpretation.DataScope, item: interpretation.DataItem): Option[T] =
        interpretation.extract(scope,item,this)

   def constant(interpretation:Interpretation)(scope: interpretation.DataScope, value:T): interpretation.DataItem =
        interpretation.constant(scope, value, this)

   def constantValue(value: T): DataSortValue[T] =
        DataSortValue(this, value) 

   def javaClass: Class[?]

   def javaRefClass: Class[?]

   /**
    * set item form java object reference.
    **/
   def javaRefConstant(interpretation:Interpretation)(scope: interpretation.DataScope, value:AnyRef): interpretation.DataItem =
      interpretation.javaRefConstant(scope, value, this)
      

trait BasicDataSortRep[T <: Matchable] extends DataSortRep[T] {

   def name: String

   def dataSort: DataSort = BasicDataSort(name)

}

object BasicDataSortRep {
   def find(name:String): BasicDataSortRep[?] =
      name match
         case IntBasicRep.name => IntBasicRep
         case LongBasicRep.name => LongBasicRep
         case FloatBasicRep.name => FloatBasicRep
         case BooleanBasicRep.name => BooleanBasicRep
         case DoubleBasicRep.name => DoubleBasicRep
         case UnitBasicRep.name => UnitBasicRep
         case _ =>
            throw new IllegalArgumentException(s"Basic data sort for $name is not known")
}


object IntBasicRep extends BasicDataSortRep[Int] {

   val name: String = "int"

   val javaClass = classOf[Int]

   val javaRefClass = classOf[java.lang.Integer]

}
given DataSortRep[Int] = IntBasicRep


object LongBasicRep extends BasicDataSortRep[Long] {

   val name: String = "long"

   val javaClass = classOf[Long]

   val javaRefClass = classOf[java.lang.Long]

}
given DataSortRep[Long] = LongBasicRep


object BooleanBasicRep extends BasicDataSortRep[Boolean] {

   val name: String = "boolean"

   val javaClass = classOf[Boolean]

   val javaRefClass = classOf[java.lang.Boolean]


}

given DataSortRep[Boolean] = BooleanBasicRep


object FloatBasicRep extends BasicDataSortRep[Float] {

   val name: String = "float"

   val javaClass = classOf[Float]

   val javaRefClass = classOf[java.lang.Float]

}
given DataSortRep[Float] = FloatBasicRep

object DoubleBasicRep extends BasicDataSortRep[Double] {

   val name: String = "double"

   val javaClass = classOf[Double]

   val javaRefClass = classOf[java.lang.Double]

}
given DataSortRep[Double] = DoubleBasicRep

object UnitBasicRep extends BasicDataSortRep[Unit] {

   val name: String = "unit"

   val javaClass = classOf[Unit]

   val javaRefClass = classOf[java.lang.Void]


}
given DataSortRep[Unit] = UnitBasicRep

trait RefDataSortRep[T <: AnyRef]:
   this: DataSortRep[T] =>
   def javaClass: Class[T]  
   def javaRefClass: Class[T] = javaClass

sealed trait TupleDataSortRep[T <: Tuple] extends DataSortRep[T]:
   def length: Int
   def items: List[DataSortRep[?]]

object TupleDataSortRep:

   def find(elements: Seq[DataSort]): TupleDataSortRep[?] =
      elements.length match
         case 0 => EmptyTupleRep
         case 1 => Tuple1Rep(DataSort.findRep(elements(0)))
         case 2 => 
            Cartesian2Rep( DataSort.findRep(elements(0)), DataSort.findRep(elements(1)) )
         case 3 => Cartesian3Rep( DataSort.findRep(elements(0)), DataSort.findRep(elements(1)), DataSort.findRep(elements(2)) )
         case _ =>
            val h = DataSort.findRep(elements.head)
            val tl = find(elements.tail)
            ConsTupleRep(h,tl)
  

case class Tuple1Rep[A](a: DataSortRep[A]) extends TupleDataSortRep[Tuple1[A]] with RefDataSortRep[Tuple1[A]]:
   def length = 1
   val dataSort: DataSort = TupleDataSort(IndexedSeq(a.dataSort))
   val javaClass = classOf[Tuple1[A]]
   override def items = List(a)

case class Cartesian2Rep[A,B](a: DataSortRep[A], b: DataSortRep[B]) extends TupleDataSortRep[(A,B)] with RefDataSortRep[(A,B)]:
   def length = 2
   val dataSort: DataSort = TupleDataSort(IndexedSeq(a.dataSort, b.dataSort))
   val javaClass: Class[Tuple2[A,B]] = classOf[Tuple2[A,B]] 
   override def items = List(a,b)


given cartesian2Rep[A,B](using a: DataSortRep[A], b: DataSortRep[B]): TupleDataSortRep[(A,B)] =
   Cartesian2Rep(a,b)


case class Cartesian3Rep[A,B,C](a: DataSortRep[A], b: DataSortRep[B], c:DataSortRep[C]) extends TupleDataSortRep[(A,B,C)] with RefDataSortRep[(A,B,C)]:
   def length = 3
   val dataSort: DataSort = TupleDataSort(IndexedSeq(a.dataSort, b.dataSort, c.dataSort))
   val javaClass: Class[Tuple3[A,B,C]] = classOf[Tuple3[A,B,C]]
   override def items = List(a,b,c)

object Cartesian3Rep:
   
   given cartesian3Rep[A,B, C](using a: DataSortRep[A], b: DataSortRep[B], c:DataSortRep[C]): TupleDataSortRep[(A,B,C)] =
      Cartesian3Rep(a,b,c)


sealed trait TupleNRep[T<:Tuple] extends TupleDataSortRep[T] {

   def items: List[DataSortRep[?]]

}

object TupleNRep {

   given TupleNRep[EmptyTuple] = EmptyTupleRep

   given [H, TL<:Tuple](using hRep:DataSortRep[H], tlRep:TupleNRep[TL]):TupleNRep[H *: TL] =
      ConsTupleRep[H,TL](hRep, tlRep)

}

case object EmptyTupleRep extends TupleNRep[EmptyTuple]:
   override def length = 0
   override def items = Nil
   override def dataSort: DataSort = TupleDataSort(IndexedSeq.empty)
   override def javaClass = classOf[Unit]
   override def javaRefClass = classOf[Void]

case class ConsTupleRep[H, TL<:Tuple](hRep: DataSortRep[H], tlRep:TupleDataSortRep[TL]) extends TupleNRep[H *: TL]:
   override def length = tlRep.length + 1
   override def items = hRep::(tlRep.items) 
   override def dataSort: DataSort = TupleDataSort(items.map(_.dataSort).toIndexedSeq)
   override def javaClass = classOf[Tuple]
   override def javaRefClass = classOf[Tuple]   


object TensorDataSortRep:

   def find(element: DataSort, flawor: TensorDataSortFlawor): DataSortRep[?] =
      tensorArrayDataSortRep(using DataSort.findRep(element))



given tensorArrayDataSortRep[E](using e:DataSortRep[E]): DataSortRep[Array[E]] with RefDataSortRep[Array[E]] with
   val  dataSort: DataSort = TensorDataSort(e.dataSort, TensorDataSortFlawor.Dence)
   val  javaClass: Class[Array[E]] = classOf[Array[E]]


import scala.collection.mutable.ArrayBuffer   

given tensorArrayBufferDataSortRep[E](using e:DataSortRep[E]): DataSortRep[ArrayBuffer[E]] with RefDataSortRep[ArrayBuffer[E]] with
   val dataSort: DataSort = TensorDataSort(e.dataSort, TensorDataSortFlawor.Dence)
   val  javaClass: Class[ArrayBuffer[E]] = classOf[ArrayBuffer[E]]


given tensorTensorDataSortRep[E](using e: DataSortRep[E]): DataSortRep[Tensor[E]] with RefDataSortRep[Tensor[E]] with
   val dataSort: DataSort = TensorDataSort(e.dataSort, TensorDataSortFlawor.Dence)
   val  javaClass: Class[Tensor[E]] = classOf[Tensor[E]]

   

object NamedSetRep:

   import java.util.concurrent.ConcurrentHashMap

   case class CaseClassRecord(className: String, elements: Map[String,DataSortRep[?]], instance: DataSortRep[?])

   // we assumw, that register is called during deriving struct
   // this is global value, but the content of the value is generated compile-time when deriving
   // and added here when soe deriving value is registered
   val structures: ConcurrentHashMap[Map[String,DataSort],CaseClassRecord] = new ConcurrentHashMap() 

   def find(sort: NamedSet): DataSortRep[?] =
      val candidate = structures.get(sort.elements)
      if (candidate eq null) then
         UniversalNamedSetRep.create(sort)
      else
         candidate.instance


case class UniversalNamedSetRep(override val dataSort: NamedSet, elements: Map[String, DataSortRep[?]]) extends DataSortRep[Map[String,Any]] with RefDataSortRep[Map[String,Any]] {
   def javaClass: Class[Map[String,Any]] = classOf[Map[String,Any]]
}

object UniversalNamedSetRep {

   def create(sort: NamedSet): UniversalNamedSetRep = {
      val repElements = sort.elements.map((name,element) => (name, DataSort.findRep(element)))
      UniversalNamedSetRep(sort, repElements)
   }

}

