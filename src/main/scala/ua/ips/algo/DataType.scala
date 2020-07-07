package ua.ips.algo

sealed trait DataType
case class BasicDataType(name:String) extends DataType
case class Cartesian(elements: IndexedSeq[DataType]) extends DataType
case class NamedSet(elements: Map[String,DataType]) extends DataType

trait DataTypeRep[T] {
   
   def dataType: DataType

   def extractItem(interpretation:Interpretation)(item: interpretation.DataItem): Option[T] =
        interpretation.extract(item,this)

   def constant(interpretation:Interpretation)(value:T): interpretation.DataItem =
        interpretation.constant(value, this)

}

trait BasicDataTypeRep[T] extends DataTypeRep[T] {

   def name: String

   def dataType: DataType = BasicDataType(name)

}

object IntBasicRep extends BasicDataTypeRep[Int] {

   def name: String = "int"

}

given DataTypeRep[Int] = IntBasicRep

object BooleanBasicRep extends BasicDataTypeRep[Boolean] {

   def name: String = "boolean"

}

given DataTypeRep[Boolean] = BooleanBasicRep

case class Cartesian2Rep[A,B](a: DataTypeRep[A], b: DataTypeRep[B]) extends DataTypeRep[(A,B)]:

   val dataType: DataType = Cartesian(IndexedSeq(a.dataType, b.dataType))


given cartesian2Rep[A,B](using a: DataTypeRep[A], b: DataTypeRep[B]) as DataTypeRep[(A,B)] =
   Cartesian2Rep(a,b)


case class FixedArrayDataType[E](length:Int, element:DataType) extends DataTypeRep[Array[E]]:

   val dataType: DataType = Cartesian(IndexedSeq.fill(length)(element))


