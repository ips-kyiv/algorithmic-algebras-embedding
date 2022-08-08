package ua.ips.algo.meta

import ua.ips.algo.*

/**
 * This is description of AST tre.
 * Case types hierarchy is mapped to Metaschema
 **/
sealed trait MetaSchema {
  def  domain:  String
  def  name:    String

  //def isA[T](value:T): T
}

/**
 * OneOf
 **/
case class SumMetaSchema(domain: String, name: String, variants: List[MetaSchema]) extends MetaSchema


/**
 * Meta
 **/
case class ProductMetaSchema(domain: String, name: String, items: List[MetaSchema]) extends MetaSchema

/**
 *
 **/
sealed trait GenericDomainMetaSchema extends MetaSchema {
   def domain: String = "generic"
}

sealed trait InputMetaSchema extends GenericDomainMetaSchema

case class TypedInputMetaSchema(
   val sort: DataSort
)  extends GenericDomainMetaSchema {
   def name: String = TypedInputMetaSchema.name
}

object TypedInputMetaSchema {
   def name: String = "typedInput"
}

case object UntypedInputMetaSchema extends GenericDomainMetaSchema {
   def name: String = "untypedInput"
}

sealed trait DataExpressionMetaSchema extends GenericDomainMetaSchema

case class TypeDataExpressionMetaSchema(
   val sort: DataSort
)  extends DataExpressionMetaSchema {
   def name: String = TypeDataExpressionMetaSchema.name
}

object TypeDataExpressionMetaSchema {
   def name: String = "typedDataExpression"
}


case object UntypedDataExpressionMetaSchema extends DataExpressionMetaSchema {
   def name: String = "untypedDataEzpression"
}


sealed trait OutputMetaSchema extends GenericDomainMetaSchema 


case class TypedOutputMetaSchema(val sort: DataSort) extends OutputMetaSchema {
   def name: String = "typedOutput"
}


case object UntypedOutputMetaSchema extends GenericDomainMetaSchema {
   def name: String = "untypedOutput"
}

