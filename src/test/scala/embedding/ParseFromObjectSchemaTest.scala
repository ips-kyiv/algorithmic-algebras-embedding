package embedded

import ua.ips.algo.*
import ua.ips.algo.given
import ua.ips.algo.runtime.*

import munit.*

trait SchemaBuilder

object ExampleSchemaHolder {

  /*
  val matrixMultiply = Schema.build{
    (x:Tensor[Double],y:Tensor[Double]) => 
      assert(x.nDims == 2 && y.nDims == 2 && x.dim(0) == y.dim(1) )
      val N = x.dim(0)
      ???
  } 
  */ 

  inline def vectorMultiply(x:Tensor1D[Double],y:Tensor1D[Double])(using SchemaBuilder): Tensor[Double] = {
      ???
  }


}
