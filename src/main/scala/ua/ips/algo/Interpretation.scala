package ua.ips.algo

class InterpretationException(message: String, ex: Throwable=null) extends SchemaException(message, ex)


trait Interpretation {

  type DataItem

  type DataScope <: AutoCloseable

  
  def extract[T](scope: DataScope, item: DataItem, rep: DataSortRep[T]): Option[T]
  

  def constant[T](scope: DataScope, value: T, rep: DataSortRep[T]): DataItem 


  def javaRefConstant(scope: DataScope, value: AnyRef, rep: DataSortRep[?]): DataItem

  /**
   * interpert signature or throw exception is one is not implemented.
   * Parameters are passed in internal encoding
   **/
  def applyInternal(scope: DataScope, signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem

  /**
   * create new data-scope
   **/
  def newDataScope(): DataScope


  /**
   * interpert signature or throw exception is one is not implemented.
   * Parameters are passed as universal values.
   **/
  def apply(signature: TypesOnlyDataSortSignature, args: Seq[DataSortValue[?]]): DataSortValue[?] =
    val scope = newDataScope()
    try
      val items = args.map( a => a.constantFor(this)(scope) )
      val itemRetval = applyInternal(scope, signature, items)
      val retRep = DataSort.findRep(signature.out)
      extract(scope, itemRetval, retRep) match
        case Some(v) => DataSortValue(retRep, v)
        case None =>
          throw new InterpretationException("return value is unrepresentable in the host")
    finally
      scope.close()


}

object DummyDataScope extends AutoCloseable {

  override def close(): Unit = {}

}


trait FreeInterpretation extends Interpretation { 

  sealed trait FreeDataItem 

  case class ConstantDataItem[T](dataSort: DataSort, value: T) extends FreeDataItem

  case class UninterpretedFunctionDataItem(signature: TypesOnlyDataSortSignature, 
                                           args: Seq[DataItem]) extends FreeDataItem:
    def  dataSort = signature.out
                       

  override type DataItem = FreeDataItem

  override type DataScope = DummyDataScope.type

  override def newDataScope(): DataScope = DummyDataScope

  override def extract[T](scope: DataScope, item: DataItem, rep: DataSortRep[T]): Option[T] =
    rep match
      case BasicDataSort(name) =>
              item match
                case constant: ConstantDataItem[_] =>
                            extractPrimitive[T](name, constant) 
                case f: UninterpretedFunctionDataItem => None
      case _ => 
              // TODO: build deriving typeclasses for scala
              ???

  override def constant[T](scope: DataScope, value: T, rep: DataSortRep[T]): DataItem =
    ConstantDataItem[T](rep.dataSort, value) 

  override def javaRefConstant(scope: DataScope, value: AnyRef, rep: DataSortRep[?]): DataItem =
    ConstantDataItem(rep.dataSort, value)
    
  
  override def applyInternal(scope: DataScope, signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem =
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

