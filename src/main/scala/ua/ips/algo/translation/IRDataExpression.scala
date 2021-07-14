package ua.ips.algo.translation

import ua.ips.algo._

sealed trait IRDataExpression {
   def id: String
   def sort: DataSort
   def origin: DataExpression
   def costEstimation: Long
} 

// TODO: implement as sealed trait wh references to var
case class IRDataExpressionOrigin(id: String, origin: DataExpression) extends IRDataExpression {
  def sort = origin.sort
  def costEstimation = 0
}


   