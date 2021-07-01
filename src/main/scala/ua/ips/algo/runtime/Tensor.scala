package ua.ips.algo.runtime

/**
 *  Tensor is a N-dimensional reassible array.
 **/
trait Tensor[E] {

    def nDims: Int
    def dim(index:Int): Int
    
    def element(indexes: Int*): E
    def update(value: E, indexes: Int*): Unit
    def swap(value: E, indexes: Int*): E

    def getElement(indexes: Int*): Option[E]

}