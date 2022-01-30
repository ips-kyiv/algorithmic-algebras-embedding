package ua.ips.algo

import scala.quoted._


case class SchemaBase(
  sorts: Set[DataSort], 
  signatures: Set[DataSortSignature])

/**
 * representation of AlgoSchema (without name and package.)
 *@see SchemaModule - as schemw aith anme and package.
 **/
sealed trait Schema:
  
  /**
   * return DataExpression is this is an OutputSchema, otherwise - throw SchemaBuildExpression
   **/
  def asDataExpression: DataExpression =
        throw SchemaBuildException(s"DataExpression expected, we have ${this}")

  def lift(using Quotes): Expr[Schema]
 
  def extractSignature(packageName:Seq[String], name:String): DataSortSignature =
    val (in, optOut) = fulfillSignature(packageName, name, Seq.empty, None)
    val out = optOut.getOrElse(BasicDataSort(UnitBasicRep.name))
    DataSortSignature(packageName, name, in, out)

  def fulfillSignature(packageName: Seq[String], name: String, in: Seq[(String,DataSort)], out:Option[DataSort]): (Seq[(String,DataSort)],Option[DataSort])



// ? - List instead pair 
case class SequentialSchema(x: Schema, y: Schema) extends Schema:

  override def asDataExpression: DataExpression = y.asDataExpression

  def lift(using Quotes): Expr[Schema] = '{SequentialSchema(${x.lift}, ${y.lift})}

  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
    val (inx, outx) = x.fulfillSignature(packageName, name, in, out)
    val (iny, outy) = y.fulfillSignature(packageName, name, inx, outx)
    (iny, outy)

    

case class ParallelSchema(x: Schema, y: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{ParallelSchema(${x.lift}, ${y.lift})}


  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
    val (inx, outx) = x.fulfillSignature(packageName, name, Seq.empty, None)
    val (iny, outy) = y.fulfillSignature(packageName, name, Seq.empty, None)
    // for now we assume that both branches shpudl have input and shoutl have the same output
    if (inx != iny) {
      throw new SchemaSignatureBuildException("Input mismatch in parallel branches"); // this.
    }
    if (outx != outy) {
      throw new SchemaSignatureBuildException("Output mismatch in parallel branches");
    }
    (in ++ inx, out.flatMap(_ => outx))


case class ConditionalSchema(cond: Condition, ifTrue: Schema, ifFalse: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{ConditionalSchema(${cond.lift}, ${ifTrue.lift}, ${ifFalse.lift})}

  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
    val (inx, outx) = ifTrue.fulfillSignature(packageName, name, Seq.empty, None)
    val (iny, outy) = ifFalse.fulfillSignature(packageName, name, Seq.empty, None)
    if (inx != iny) {
      throw new SchemaSignatureBuildException("Input mismatch in parallel branches"); // this.
    }
    if (outx != outy) {
      throw new SchemaSignatureBuildException("Output mismatch in parallel branches");
    }
    (in ++ inx, out.flatMap(_ => outx))




case class LoopSchema(cond: Condition, body: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{LoopSchema(${cond.lift}, ${body.lift})}

  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
      body.fulfillSignature(packageName, name, in, out)


case class ParallelIterationSchema(varName: Name,
                                      start: DataExpression, 
                                      end: DataExpression,
                                      workSize: DataExpression,
                                      body: Schema) extends Schema:

  def lift(using Quotes): Expr[Schema] = '{ 
    ParallelIterationSchema( ${Expr(varName)}, ${start.lift}, ${end.lift}, ${workSize.lift}, ${body.lift} ) 
  }
  
  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
    body.fulfillSignature(packageName, name, in, out)


case class AssertSchema(cond: Condition) extends Schema:
  def lift(using Quotes): Expr[Schema] = '{AssertSchema(${cond.lift})}

  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
    (in, out)

case class InputSchema(parameters: Seq[InputSchema.Entry]) extends Schema:
  
  def lift(using Quotes): Expr[Schema] = '{ InputSchema(${Expr.ofSeq(parameters.map(_.lift))}) }

  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
    (in.appendedAll(parameters.map(x => (x.variable,x.sort))), out)


object InputSchema{

  case class Entry(variable: Name, sort: DataSort) {
    def lift(using Quotes): Expr[InputSchema.Entry] = '{InputSchema.Entry(${Expr(variable)}, ${sort.lift} )}
  }

}

case class AssignSchema(variable: Name, expr: DataExpression) extends Schema:
  def lift(using Quotes): Expr[Schema] = '{AssignSchema(${Expr(variable)}, ${expr.lift} )}
  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
        (in, out)


case class OutputSchema(expr: DataExpression) extends Schema:
  override def asDataExpression: DataExpression = expr
  def lift(using Quotes): Expr[Schema] = '{OutputSchema(${expr.lift})}
  def fulfillSignature(packageName: Seq[String], name:String, in: Seq[(String,DataSort)], out:Option[DataSort] ): (Seq[(String,DataSort)],Option[DataSort]) =
    out match
      case None => (in, Some(expr.sort))
      case Some(x) => throw SchemaSignatureBuildException("double output")


object Schema {

  inline def build[A](inline f: A): Schema = ${
      SchemaEmbedding.buildImpl[A]('f)
  }

}


