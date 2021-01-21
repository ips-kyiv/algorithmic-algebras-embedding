package ua.ips.algo

import scala.annotation._

case class ParRange(range: Range, workGroupSize: Int):

  @compileTimeOnly("ParRange should be used only inside algorithm schemas")
  def foreach[U](x:Int => U):Unit = ???


extension (r:Range)
  def par(n:Int): ParRange =
    ParRange(r,n)

