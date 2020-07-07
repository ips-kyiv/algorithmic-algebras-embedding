package ua.ips.algo


trait Interpretation {

  trait DataItemType {
     type ScalaType;
     def dataType: DataType;
  }

  type DataItem <: DataItemType

  def extract[T](item: DataItem, rep: DataTypeRep[T]): Option[T]

  def constant[T](value: T, rep: DataTypeRep[T]): DataItem 

  //def apply(x:Signature, args: Seq[DataItem]): DataItem

}


class DirectInterpretation extends Interpretation {

  case class ConstantDataItem[T](override val dataType: DataType, value: T) extends DataItemType:
    type ScalaType = T

  override type DataItem = ConstantDataItem[?]

  override def extract[T](item: DataItem, rep: DataTypeRep[T]): Option[T] =
    rep match
      case BasicDataType(name) =>
              extractPrimitive[T](name, item) 
      case _ => ???

  override def constant[T](value: T, rep: DataTypeRep[T]): DataItem =
    ConstantDataItem[T](rep.dataType, value) 
  

  def extractPrimitive[T](name: String, item: ConstantDataItem[?]): Option[T] =
     item.dataType match
       case BasicDataType(itemName) if itemName == name =>
           Some(item.value).asInstanceOf[Option[T]]
       case _ =>
           None

}

object DirectInterpretation extends DirectInterpretation

// TODO: Staged.

