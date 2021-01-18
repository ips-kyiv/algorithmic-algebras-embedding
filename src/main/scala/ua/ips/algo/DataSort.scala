package ua.ips.algo

import scala.quoted._

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

case class Cartesian2Rep[A,B](a: DataSortRep[A], b: DataSortRep[B]) extends DataSortRep[(A,B)]:

   val dataSort: DataSort = Cartesian(IndexedSeq(a.dataSort, b.dataSort))


given cartesian2Rep[A,B](using a: DataSortRep[A], b: DataSortRep[B]): DataSortRep[(A,B)] =
   Cartesian2Rep(a,b)


case class FixedArrayDataSort[E](length:Int, element:DataSort) extends DataSortRep[Array[E]]:

   val dataSort: DataSort = Cartesian(IndexedSeq.fill(length)(element))


