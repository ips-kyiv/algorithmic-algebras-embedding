package ua.ips.algo.meta

import ua.ips.algo.*

/**
 * Gemerelaized AST tree, which can be used as IR-representation and AST
 **/
trait MetaSchemaInstance {
    def metaSchema:  MetaSchema
    def pos: SourcePosition;
}


case class ProductMetaSchemaInstance(
    override val metaSchema: MetaSchema,
    override val pos: SourcePosition,
    items: IndexedSeq[MetaSchemaInstance]
) extends MetaSchemaInstance


/**
 * Temporary node, can be deleted
 **/
case class SumMetaSchemaIstance(
    override val metaSchema: MetaSchema,
    override val pos: SourcePosition,
    item: MetaSchemaInstance
) extends MetaSchemaInstance

/**
 * Schema instance, correspond to data expression
 **/
case class DataExpressionMetaSchemaInstance(
    override val metaSchema: DataExpressionMetaSchema,
    override val pos: SourcePosition,
    expression: DataExpression
)  extends MetaSchemaInstance

