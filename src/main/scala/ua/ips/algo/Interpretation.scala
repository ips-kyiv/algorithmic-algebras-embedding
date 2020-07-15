package ua.ips.algo


trait Interpretation {

  trait DataItemType {
     def dataSort: DataSort;
  }

  type DataItem <: DataItemType

  def extract[T](item: DataItem, rep: DataSortRep[T]): Option[T]

  def constant[T](value: T, rep: DataSortRep[T]): DataItem 

  // interpert signature or throw exception is one is not implemented
  def apply(signature:DataSortSignature, args: Seq[DataItem]): DataItem

}


trait FreeInterpretation extends Interpretation {

  sealed trait FreeDataItem extends DataItemType

  case class ConstantDataItem[T](override val dataSort: DataSort, value: T) 
                                                                  extends FreeDataItem

  case class UninterpretedFunctionDataItem(signature:DataSortSignature, 
                                           args: Seq[DataItem]) extends FreeDataItem:
    def  dataSort = signature.out
                       

  override type DataItem = FreeDataItem

  override def extract[T](item: DataItem, rep: DataSortRep[T]): Option[T] =
    rep match
      case BasicDataSort(name) =>
              item match
                case constant: ConstantDataItem[_] =>
                            extractPrimitive[T](name, constant) 
                case f: UninterpretedFunctionDataItem => None
      case _ => 
              // TODO: build deriving typeclasses for scala
              ???

  override def constant[T](value: T, rep: DataSortRep[T]): DataItem =
    ConstantDataItem[T](rep.dataSort, value) 
  
  override def apply(signature:DataSortSignature, args: Seq[DataItem]): DataItem =
    //TODO: check typing
    // TODO: add term rewriting in non-free
    UninterpretedFunctionDataItem(signature, args)


  def extractPrimitive[T](name: String, item: ConstantDataItem[?]): Option[T] =
     item.dataSort match
       case BasicDataSort(itemName) if itemName == name =>
           Some(item.value).asInstanceOf[Option[T]]
       case _ =>
           None

}

object FreeInterpretation extends FreeInterpretation

// TODO: Staged.

