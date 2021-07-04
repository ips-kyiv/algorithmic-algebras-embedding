package ua.ips.algo

import ua.ips.algo.runtime.*

import scala.compiletime.*
import scala.quoted.*


sealed trait DataSort:
  def lift(using Quotes): Expr[DataSort]

case class BasicDataSort(name:String) extends DataSort:
  def lift(using Quotes): Expr[DataSort] = '{ BasicDataSort(${Expr(name)}) }

case class Cartesian(elements: IndexedSeq[DataSort]) extends DataSort:
  def lift(using Quotes) = '{ Cartesian(${Expr.ofSeq(elements.map(_.lift))}.toIndexedSeq) }

case class NamedSet(elements: Map[String,DataSort]) extends DataSort:
  def lift(using Quotes): Expr[DataSort] =
       '{ NamedSet( ${Expr.ofSeq(
              elements.toSeq.map(x => Expr.ofTupleFromSeq(Seq(Expr(x._1),x._2.lift)).asExprOf[(String,DataSort)])
        )}.toMap ) }

case class FixedArrayDataSort(length: Int, element: DataSort) extends DataSort:
   def lift(using Quotes): Expr[DataSort] =
      '{  FixedArrayDataSort( ${Expr(length)} , ${element.lift} ) }


enum TensorDataSortFlawor:
      case Dence, Diagonal, AllSame   

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
     


trait DataSortRep[T]:
   
   def dataSort: DataSort

   def extractItem(interpretation:Interpretation)(item: interpretation.DataItem): Option[T] =
        interpretation.extract(item,this)

   def constant(interpretation:Interpretation)(value:T): interpretation.DataItem =
        interpretation.constant(value, this)


trait BasicDataSortRep[T] extends DataSortRep[T] {

   def name: String

   def dataSort: DataSort = BasicDataSort(name)

}


object IntBasicRep extends BasicDataSortRep[Int] {

   val name: String = "int"

}

given DataSortRep[Int] = IntBasicRep

object BooleanBasicRep extends BasicDataSortRep[Boolean] {

   val name: String = "boolean"

}

given DataSortRep[Boolean] = BooleanBasicRep


object FloatBasicRep extends BasicDataSortRep[Float] {

   val name: String = "float"

}
given DataSortRep[Float] = FloatBasicRep

object DoubleBasicRep extends BasicDataSortRep[Double] {

   val name: String = "double"

}
given DataSortRep[Double] = DoubleBasicRep


case class Cartesian2Rep[A,B](a: DataSortRep[A], b: DataSortRep[B]) extends DataSortRep[(A,B)]:

   val dataSort: DataSort = Cartesian(IndexedSeq(a.dataSort, b.dataSort))


given cartesian2Rep[A,B](using a: DataSortRep[A], b: DataSortRep[B]): DataSortRep[(A,B)] =
   Cartesian2Rep(a,b)


case class FixedArrayDataSortRep[E, N <:Int](length:N, element:DataSort) extends DataSortRep[FixedArray[E,N]]:

   //val dataSort: DataSort = Cartesian(IndexedSeq.fill(length)(element))
   val dataSort: DataSort = FixedArrayDataSort(length, element)

inline given FixedArray2Rep[E,N <: Int](using e: DataSortRep[E]): DataSortRep[FixedArray[E,N]] =
   inline constValueOpt[N] match
      case Some(n) => FixedArrayDataSortRep(n, e.dataSort)
      case None => error("N is not a const-value")


 

 


