package ua.ips.algo

import scala.quoted._

sealed trait DataExpression:
  def sort: DataSort
  def lift(using Quotes): Expr[DataExpression]

case class DataAccessExpression(name: Name, sort: DataSort) extends DataExpression:
  def lift(using Quotes): Expr[DataExpression] =
    '{ DataAccessExpression(${Expr(name)}, ${sort.lift}) }

case class FunctionalExpression(signature: DataSortSignature, 
                                args: Seq[DataExpression]) extends DataExpression:
  def sort = signature.out 
  def lift(using Quotes): Expr[DataExpression] =
    '{ FunctionalExpression(
         ${signature.lift},
         ${Expr.ofSeq( args.map(_.lift) ) }
       ) }

/*
case class ConstantExpression[T:Liftable:Type](sort: DataSort, value:T) extends DataExpression:

  def lift(using Quotes): Expr[DataExpression] =
    val exprV : Expr[T] = Expr(value)
    '{ ConstantExpression(${sort.lift}, ${Expr(value)}) }
*/

case class ConstantExpression(sort: DataSort, value:String) extends DataExpression:
  def lift(using Quotes): Expr[DataExpression] =
    '{ ConstantExpression(${sort.lift}, ${Expr(value)}) }




