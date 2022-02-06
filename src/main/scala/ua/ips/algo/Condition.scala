package ua.ips.algo

import scala.annotation.alpha
import scala.quoted._


sealed trait Condition:

  @alpha("and") def && (y:Condition): Condition =
     AndCondition(this, y)
      
  @alpha("or") def || (y:Condition): Condition =
     OrCondition(this, y)

  def not: Condition =
     NotCondition(this)

  def lift(using Quotes): Expr[Condition]


case class BaseCondition(expr: DataExpression, pos: SourcePosition) extends Condition:
  def lift(using Quotes): Expr[Condition] = '{ BaseCondition(${expr.lift}, ${pos.lift}) }

case class AndCondition(x:Condition, y:Condition) extends Condition:
  def lift(using Quotes): Expr[Condition] = '{AndCondition(${x.lift}, ${y.lift}) }
  
case class OrCondition(x:Condition, y:Condition) extends Condition:
  def lift(using Quotes): Expr[Condition] = '{OrCondition(${x.lift}, ${y.lift}) }

case class NotCondition(x:Condition) extends Condition:
  def lift(using Quotes): Expr[Condition] = '{ NotCondition(${x.lift}) }


