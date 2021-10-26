package ua.ips.algo.translation.languages.clike


// TODO: set jdk-option to enable 
import jdk.incubator.foreign.*
import jdk.incubator.foreign.CLinker.VaList.Builder

import ua.ips.algo.*
import ua.ips.algo.translation.*


class Jep412Caller(signature: DataSortSignature, address: MemoryAddress, variant: Seq[String]) extends Interpretation {

  // TODO: think about resources deallocation. Mb adress in pool.
  type DataItem = MemoryAddress

  def extract[T](item: DataItem, rep: DataSortRep[T]): Option[T] = ???

  def constant[T](value: T, rep: DataSortRep[T]): DataItem = ???

  // interpert signature or throw exception is one is not implemented
  def apply(signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem = ???


}