package ua.ips.algo

import scala.annotation.alpha


sealed trait Condition:

  @alpha("and") def && (y:Condition): Condition =
     AndCondition(this, y)
      
  @alpha("or") def || (y:Condition): Condition =
     OrCondition(this, y)

  def not: Condition =
     NotCondition(this)


object Condition:
  def base(signature: DataSortSignature, args: Seq[Name]): Condition =
     signature.out match
        case BasicDataSort(name) if name == BooleanBasicRep.name =>
          if args.size != signature.in.size then
              throw new SchemaBuildException("args mismatch")
          BaseCondition(signature, args)
        case _ =>  
          throw new SchemaBuildException("non boolean signature")


case class BaseCondition(signature: DataSortSignature, args:Seq[Name]) extends Condition

case class AndCondition(x:Condition, y:Condition) extends Condition
  
case class OrCondition(x:Condition, y:Condition) extends Condition

case class NotCondition(x:Condition) extends Condition

