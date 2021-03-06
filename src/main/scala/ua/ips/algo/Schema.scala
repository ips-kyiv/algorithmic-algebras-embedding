package ua.ips.algo

import scala.quoted._


case class SchemaBase(
  sorts: Set[DataSort], 
  signatures: Set[DataSortSignature])

/**
 * representation of AlgoSchema
 **/
sealed trait Schema:
  
  /**
   * return DataExpression is this is an OutputSchema, otherwise - throw SchemaBuildExpression
   **/
  def asDataExpression: DataExpression =
        throw SchemaBuildException(s"DataExpression expected, we have ${this}")

  def lift(using Quotes): Expr[Schema]
 
// ? - List instead pair 
case class SequentialSchema(x: Schema, y: Schema) extends Schema:

  override def asDataExpression: DataExpression = y.asDataExpression

  def lift(using Quotes): Expr[Schema] = '{SequentialSchema(${x.lift}, ${y.lift})}


case class ParallelSchema(x: Schema, y: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{ParallelSchema(${x.lift}, ${y.lift})}


case class ConditionalSchema(cond: Condition, ifTrue: Schema, ifFalse: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{ConditionalSchema(${cond.lift}, ${ifTrue.lift}, ${ifFalse.lift})}


case class LoopSchema(cond: Condition, body: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{LoopSchema(${cond.lift}, ${body.lift})}


case class ParallelIterationSchema(varName: Name,
                                      start: DataExpression, 
                                      end: DataExpression,
                                      workSize: DataExpression,
                                      body: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{ 
    ParallelIterationSchema( ${Expr(varName)}, ${start.lift}, ${end.lift}, ${workSize.lift}, ${body.lift} ) 
  }
  


case class AssertSchema(cond: Condition) extends Schema:
  def lift(using Quotes): Expr[Schema] = '{AssertSchema(${cond.lift})}



//
case class InputSchema(parameters: Seq[InputSchema.Entry]) extends Schema:
  def lift(using Quotes): Expr[Schema] = '{ InputSchema(${Expr.ofSeq(parameters.map(_.lift))}) }

object InputSchema{

  case class Entry(variable: Name, sort: DataSort) {
    def lift(using Quotes): Expr[InputSchema.Entry] = '{InputSchema.Entry(${Expr(variable)}, ${sort.lift} )}
  }

}

case class AssignSchema(variable: Name, expr: DataExpression) extends Schema:
  def lift(using Quotes): Expr[Schema] = '{AssignSchema(${Expr(variable)}, ${expr.lift} )}

case class OutputSchema(expr: DataExpression) extends Schema:
  override def asDataExpression: DataExpression = expr
  def lift(using Quotes): Expr[Schema] = '{OutputSchema(${expr.lift})}


object Schema {

  inline def build[A](inline f: A): Schema = ${
      SchemaEmbedding.buildImpl[A]('f)
  }

}


