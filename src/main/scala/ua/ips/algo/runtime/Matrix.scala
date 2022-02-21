package ua.ips.algo.runtime

import scala.reflect.ClassTag

type Matrix[E] = Tensor2D[E]

/**
 * 2D tensor, i.e. matrix
 */
trait Tensor2D[E] extends Tensor[E] {

    def nDims = 2

    def dimX: Int
    def dimY: Int

    def elementN(indexes: Int*): E =
        assert(indexes.length == 2)
        element2(indexes(0),indexes(1))

    def getElementN(indexes:Int*): Option[E] =
      if (indexes.length != 2) {
        None
      } else {
        val x = indexes(0)
        val y = indexes(1)
        if (x >= dimX  || x < 0||y >= dimY || y < 0) {
          None
        } else {
          Some(element2(x,y))
        }
      }

    def element2(x: Int, y: Int): E
    def update2(value: E, x: Int, y:Int): E

    def apply(x:Int, y:Int): E = element2(x,y)
    def update(x:Int, y:Int, value:E): Unit

}

/*
class GenericTensor2D[E:ClassTag](initDimX: Int, initDimY:Int, data: Array[E]) extends Tensor2D[E] {

  protected var _dimX = initDimX
  protected var _dimY = initDimY

  def dimX: Int = _dimX
  def dimY: Int = _dimY




}
*/

