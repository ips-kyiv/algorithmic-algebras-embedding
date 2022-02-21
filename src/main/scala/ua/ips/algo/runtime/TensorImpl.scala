package ua.ips.algo.runtime

import scala.reflect.ClassTag

trait GenericTensorBase[E](initDimData: Seq[Int]) extends Tensor[E] {

  protected var dimData = initDimData

  def plainIndex(indexes: Seq[Int]): Int = {
    var ii = 0
    var ri = 0
    while(ii < indexes.length) {
      if (ii > 0) {
        ri = ri * dimData(ii-1)
      }
      val di = indexes(ii)
      if (di >= dimData(ii) ) {
        throw new IndexOutOfBoundsException(s"IllegalIndex ii=$ii, di=$di when maxValus shpould be ${indexes(ii)}")
      }
      ri = ri + indexes(ii)
      ii = ii + 1
    }
    ri
  }

  def reshapeInplace(newDimData: Seq[Int]) = {
    var oldProd = 1
    var newProd = 1
    val maxDimLen = if dimData.length >= newDimData.length then dimData.length else newDimData.length
    var i=0
    while(i < maxDimLen) {
      // TODO: check overflow
      if (i < dimData.length) then
        oldProd = oldProd * dimData(i)
      if (i < newDimData.length) then
        newProd = newProd * newDimData(i)
      i = i+i
    }
    if (oldProd != newProd) {
      throw new IllegalArgumentException(s"New dimension ${newDimData} not match existing ${dimData}")
    }
    dimData = newDimData
  } 



}


class GenericTensorImpl[E:ClassTag](initDimData: Seq[Int], data: Array[E]) extends GenericTensorBase[E](initDimData) {

  def nDims = dimData.length
  def dim(i: Int) = dimData(i)

  def elementN(indexes: Int*): E = 
    val ri = plainIndex(indexes)
    data(ri)

  def getElementN(indexes: Int*): Option[E] = {
    try {
      Some(elementN(indexes: _*))
    } catch {
      case ex: IndexOutOfBoundsException => None
    }
  }

  def updateN(indexes: Seq[Int], value: E): Unit = {
    val ri = plainIndex(indexes)
    data(ri) = value
  }

  def swapN(xIndexes: Seq[Int], yIndexes: Seq[Int]): Unit = {
    val xi = plainIndex(xIndexes)
    val yi = plainIndex(yIndexes)
    val tmp = data(xi)
    data(xi)=data(yi)
    data(yi)=tmp
  }

  def copy(): Tensor[E] = {
    val newData = new Array[E](data.length)
    System.arraycopy(data, 0, newData, 0, data.length)
    GenericTensorImpl(dimData, newData)
  }

}

//class FloatGenericTensorImpl(data: Array[Array[Float]])


class DoubleGenericTensorImpl(initDimData: Seq[Int], data: Array[Double]) extends GenericTensorBase[Double](initDimData) {

    def nDims = dimData.length
    def dim(i: Int) = dimData(i)

    def elementN(indexes: Int*): Double = {
      val ri = plainIndex(indexes)
      data(ri)
    }

    def getElementN(indexes: Int*): Option[Double] = {
      try 
        Some(elementN(indexes: _*))
      catch 
        case ex: IndexOutOfBoundsException => None
    }
    
    def updateN(indexes: Seq[Int], value: Double): Unit = {
      val ri = plainIndex(indexes)
      data(ri) = value
    }
  
    def swapN(xIndexes: Seq[Int], yIndexes: Seq[Int]): Unit = {
      val xi = plainIndex(xIndexes)
      val yi = plainIndex(yIndexes)
      val tmp = data(xi)
      data(xi)=data(yi)
      data(yi)=tmp
    }
  
    def copy(): Tensor[Double] = {
      val newData = new Array[Double](data.length)
      System.arraycopy(data, 0, newData, 0, data.length)
      DoubleGenericTensorImpl(dimData, newData)
    }
  

}


