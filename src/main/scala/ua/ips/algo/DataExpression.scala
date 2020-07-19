package ua.ips.algo

sealed trait DataExpression

case class DataAccessExpression(name: Name) extends DataExpression

case class FunctionalExpression(signature: DataSortSignature, 
                                args: Seq[DataExpression]) extends DataExpression


