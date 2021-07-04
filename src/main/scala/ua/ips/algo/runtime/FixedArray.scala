package ua.ips.algo.runtime

import scala.compiletime.constValue
import scala.compiletime.ops.int.*

trait FixedArray[E,N <: Int]:

  def  apply(i:Int):E
  
  def  update(i:Int, e:E):Unit



