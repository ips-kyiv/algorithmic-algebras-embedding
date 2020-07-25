package ua.ips.algo

case class SchemaBase(sorts: Set[DataSort], signatures: Set[DataSortSignature])

sealed trait Schema
 
// ? - List instead pair 
case class SequentialSchema(x: Schema, y: Schema) extends Schema

case class ParallelSchema(x: Schema, y: Schema) extends Schema

case class ConditionalSchema(cond: Condition, ifTrue: Schema, ifFalse: Schema) extends Schema

case class LoopSchema(cond: Condition, body: Schema) extends Schema

case class AssertSchema(cond: Condition) extends Schema

//
case class InputSchema(variable: Name, sort: DataSort) extends Schema

case class AssignSchema(variable: Name, expr: DataExpression) extends Schema

case class OutputSchema(expr: DataExpression) extends Schema


object Schema {

  inline def build[A,B](inline f: A=>B): Schema = ${
      SchemaEmbedding.buildImpl[A,B]('f)
  }

}


