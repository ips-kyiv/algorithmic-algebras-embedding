package ua.ips.algo


trait Interpretation {

  trait DataItemType {
     type ScalaType;
     def dataSort: DataSort;
  }

  type DataItem <: DataItemType

  def extract[T](item: DataItem, rep: DataSortRep[T]): Option[T]

  def constant[T](value: T, rep: DataSortRep[T]): DataItem 

  //def apply(x:Signature, args: Seq[DataItem]): DataItem

}


class DirectInterpretation extends Interpretation {

  case class ConstantDataItem[T](override val dataSort: DataSort, value: T) extends DataItemType:
    type ScalaType = T

  override type DataItem = ConstantDataItem[?]

  override def extract[T](item: DataItem, rep: DataSortRep[T]): Option[T] =
    rep match
      case BasicDataSort(name) =>
              extractPrimitive[T](name, item) 
      case _ => 
              // TODO: build deriving typeclasses for scala
              ???

  override def constant[T](value: T, rep: DataSortRep[T]): DataItem =
    ConstantDataItem[T](rep.dataSort, value) 
  

  def extractPrimitive[T](name: String, item: ConstantDataItem[?]): Option[T] =
     item.dataSort match
       case BasicDataSort(itemName) if itemName == name =>
           Some(item.value).asInstanceOf[Option[T]]
       case _ =>
           None

}

object DirectInterpretation extends DirectInterpretation

// TODO: Staged.

