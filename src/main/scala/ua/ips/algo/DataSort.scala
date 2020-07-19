package ua.ips.algo

sealed trait DataSort
case class BasicDataSort(name:String) extends DataSort
case class Cartesian(elements: IndexedSeq[DataSort]) extends DataSort
case class NamedSet(elements: Map[String,DataSort]) extends DataSort

trait DataSortRep[T] {
   
   def dataSort: DataSort

   def extractItem(interpretation:Interpretation)(item: interpretation.DataItem): Option[T] =
        interpretation.extract(item,this)

   def constant(interpretation:Interpretation)(value:T): interpretation.DataItem =
        interpretation.constant(value, this)

}

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


given cartesian2Rep[A,B](using a: DataSortRep[A], b: DataSortRep[B]) as DataSortRep[(A,B)] =
   Cartesian2Rep(a,b)


case class FixedArrayDataSort[E](length:Int, element:DataSort) extends DataSortRep[Array[E]]:

   val dataSort: DataSort = Cartesian(IndexedSeq.fill(length)(element))


