package ua.ips.algo

trait SchemaBase(sorts: Set[DataSort], signatures: Set[DataSortSignature])

sealed trait Schema
 
case class AssignSchema(variable: Name, expr: DataExpression) extends Schema

case class SequentialSchema(x: Schema, y: Schema) extends Schema

case class ParallelSchema(x: Schema, y: Schema) extends Schema

case class ConditionalSchema(cond: Condition, ifTrue: Schema, ifFalse: Schema) extends Schema

case class LoopSchema(cond: Condition, body: Schema) extends Schema

case class AssertSchema(cond: Condition) extends Schema


