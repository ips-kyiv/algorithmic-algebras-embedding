package ua.ips.algo.translation

import ua.ips.algo._

// TODO: implement as sealed trait wh references to var
case class IRDataExpression(id: String, origin: DataExpression) {
  def sort = origin.sort
}


   