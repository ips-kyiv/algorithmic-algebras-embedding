package ua.ips.algo.translation.languages.clike


// TODO: set jdk-option to enable 
import jdk.incubator.foreign.*
import jdk.incubator.foreign.CLinker.VaList.Builder

import ua.ips.algo.*
import ua.ips.algo.translation.*




class Jep412Caller(  signature: DataSortSignature, mainFunAddress: MemoryAddress, variant: Seq[String]) extends Interpretation {

  // TODO: think about resources deallocation. Mb adress in pool.
  type DataItem = MemoryAddress | Int

  type DataScope = ResourceScope
  
  def newDataScope(): ResourceScope = ResourceScope.newConfinedScope()

  def extract[T](scope: ResourceScope, item: DataItem, rep: DataSortRep[T]): Option[T] = {
     ???
  }

  def constant[T](scope: ResourceScope, value: T, rep: DataSortRep[T]): DataItem = ???

  // interpert signature or throw exception is one is not implemented
  def applyInternal(scope: ResourceScope, signature: TypesOnlyDataSortSignature, args: Seq[DataItem]): DataItem = {
     val scope = ???

     ???
  }


}

