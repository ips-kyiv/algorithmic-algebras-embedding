package ua.ips.algo.runtime



type TVector[E] = Tensor1D[E]

trait Tensor1D[E] extends Tensor[E] {

    def nDims = 1

    def elementN(indexes: Int*): E =
        assert(indexes.length == 1)
        element(indexes(0))

    def element(x: Int): E
 
    def apply(x:Int): E = element(x)
    def update(x:Int, y:Int, value:E): Unit

}




