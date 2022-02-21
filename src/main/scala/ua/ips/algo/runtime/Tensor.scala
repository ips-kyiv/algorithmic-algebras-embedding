package ua.ips.algo.runtime

/**
 *  Tensor is a N-dimensional reassible array.
 **/
trait Tensor[E] {

    def nDims: Int
    def dim(index:Int): Int
    
    def elementN(indexes: Int*): E
    def updateN(indexes: Seq[Int], value: E): Unit
    def swapN(xIndexes: Seq[Int], yIndexes: Seq[Int]): Unit

    def getElementN(indexes: Int*): Option[E]

    def reshapeInplace(dims: Seq[Int]): Unit

    def copy(): Tensor[E]

}

object Tensor {

}

trait TensorView[C[_]]  {

    def asTensor[E](c:C[E]): Tensor[E]

}



