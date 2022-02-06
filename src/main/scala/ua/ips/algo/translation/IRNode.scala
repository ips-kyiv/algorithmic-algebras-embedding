package ua.ips.algo.translation

import ua.ips.algo._

/**
* schema with additional data.
**/
sealed trait IRNode{
  def  id: String;
  def  schema: Option[Schema];
  def  costEstimations(ctx: IRContext): CostEstimations = CostEstimations.ZERO;  // TODO: implement

  def  subnode(ids: Seq[String]): Option[IRNode]
  
  //def  cfNext(ctx: IRContex): Set[IRNode];
  //def  cfPrev(ctx: IRContex): Set[IRNode];
  //def  isVar: Boolean;
}


object IRNode:

    def accept(ctx: IRContext, schema: Schema, rootPath: String): IRNode =
      val node = schema match
        case s@SequentialSchema(x, y, pos) =>
          SeqIRNode.create(ctx, s, rootPath)
        case p@ParallelSchema(x,y, pos) =>
          ParIRNode.create(ctx,p, rootPath)
        case in@InputSchema(parameters, pos) =>
          IRInputs.create(ctx,in,rootPath)
        case out@OutputSchema(expr, pos) =>
          IROutput.create(ctx,out,rootPath)
      ctx.addNode(rootPath, node)
      node    


    def indexSubnode(current: IRNode, subnodes: IndexedSeq[IRNode], names: Seq[String]): Option[IRNode] = 
      if names.isEmpty then
        Some(current)
      else 
        val i = try {
          names.head.toInt
        } catch {
          case ex: NumberFormatException =>
            -1
        }
        if (i >= 0 && i < subnodes.length) then 
           subnodes(i).subnode(names.tail)
        else 
           None




end IRNode



case class SeqIRNode(id: String,
                     schema: Option[Schema],
                     internalNodes: IndexedSeq[IRNode]) extends IRNode {

    private var _costEstimantions: CostEstimations | Null = null

    override def costEstimations(ctx: IRContext): CostEstimations =
      if (_costEstimantions != null) then
        _costEstimantions.nn
      else 
        internalNodes.foldLeft(CostEstimations.ZERO)((s,e) => s.seqMerge(e.costEstimations(ctx)))
 
    def subnode(names: Seq[String]): Option[IRNode] =
      IRNode.indexSubnode(this, internalNodes, names)

        
}

object SeqIRNode:


    case class AcceptRest(nodes: IndexedSeq[IRNode], rootPath: String, seqIndex: Int)

    def create(ctx: IRContext, schema: SequentialSchema, rootPath: String): SeqIRNode =
      val r0 = AcceptRest(IndexedSeq.empty, rootPath, 0)
      SeqIRNode(rootPath, Some(schema), append(ctx, append(ctx, r0,schema.x),schema.y).nodes)
      

    def append(ctx: IRContext, prev: AcceptRest, schema: Schema): AcceptRest =
      schema match
        case SequentialSchema(x,y, pos) =>
           append(ctx,append(ctx,prev, x), y)
        case other => 
           val inner = IRNode.accept(ctx, schema, s"${prev.rootPath}.${prev.seqIndex}")
           prev.nodes.lastOption match
              case Some(last) => ctx.controlFlow.set(last.id, inner.id, true)
              case None => // do nothing
           prev.copy(nodes = prev.nodes :+ inner,  seqIndex = prev.seqIndex + 1)
           

end SeqIRNode


case class ParIRNode(
   id: String,
   schema: Option[Schema],  
   internalNodes: IndexedSeq[IRNode]
 )  extends  IRNode {

   private var _costEstimantions: CostEstimations | Null = null

   override def costEstimations(ctx: IRContext): CostEstimations =
    if (_costEstimantions != null) then
      _costEstimantions.nn
    else 
      internalNodes.foldLeft(CostEstimations.ZERO)((s,e) => s.parMerge(e.costEstimations(ctx)))

   def subnode(names: Seq[String]): Option[IRNode] =
      IRNode.indexSubnode(this, internalNodes, names)
     

}

object ParIRNode:

  case class AcceptRest(nodes: IndexedSeq[IRNode], rootPath: String, flowIndex: Int)

  def create(ctx: IRContext, schema: ParallelSchema, rootPath: String): ParIRNode =
    val r0 = AcceptRest(IndexedSeq.empty, rootPath, 0)
    ParIRNode(rootPath, Some(schema), append(ctx, append(ctx,r0,schema.x),schema.y).nodes)

  def append(ctx: IRContext, prev: AcceptRest, schema: Schema): AcceptRest =
    schema match
      case ParallelSchema(x,y, pos) =>
          append(ctx, append(ctx, prev, x), y)
      case other => 
           val inner = IRNode.accept(ctx, schema, s"${prev.rootPath}.${prev.flowIndex}")
           prev.copy(nodes = prev.nodes :+ inner, flowIndex = prev.flowIndex + 1)

end ParIRNode

case class CondIRNode(id: String, 
                    schema: Option[Schema], 
                    cond: IRDataExpression, 
                    ifTrue: IRNode, 
                    ifFalse: IRNode ) extends IRNode {

  def subnode(names: Seq[String]) =
      if (names.isEmpty) Some(this) 
      else 
        names.head match
          case "t" => Some(ifTrue)
          case "f" => Some(ifFalse)
          case _ => None
          
          
  override def costEstimations(ctx: IRContext): CostEstimations =
    ifTrue.costEstimations(ctx).altMerge(ifFalse.costEstimations(ctx))


}



case class EmptyIRNode(id: String) extends IRNode:

  def schema: Option[Schema] = None

  def subnode(names: Seq[String]) =
    if (names.isEmpty) Some(this) else None

  override def costEstimations(ctx: IRContext) = CostEstimations.ZERO


case class IRVar(id: String,
                 schema: Option[Schema],
                 name: String,
                 init: IRDataExpression) extends IRNode:

  def subnode(names: Seq[String]) =
    if (names.isEmpty) Some(this) else None

  override def costEstimations(ctx: IRContext) = CostEstimations.ZERO




case class IRInputs(id: String,
                    schema: Option[Schema],
                    inputs: IndexedSeq[IRVar]) extends IRNode:

  def subnode(names: Seq[String]): Option[IRNode] =
    if (names.isEmpty) then
       Some(this) 
    else 
      try
        val index = names.head.toInt
        if (index >=0 && index < names.length) then
            inputs(index).subnode(names.tail)
        else None
      catch
            case ex: NumberFormatException => None
          

              
object IRInputs:

  def create(ctx: IRContext, schema: InputSchema, rootPath: String): IRInputs = 
      val inputsId = rootPath
      val irParams = schema.parameters.zipWithIndex.map( (p,i) =>
          val id = s"${inputsId}.${p.variable}"
          val dataExpression = DataInputExpression(p.variable, p.sort, i)
          IRVar(id,None,p.variable, IRDataExpressionOrigin(s"${id}.right", dataExpression))
      ).toIndexedSeq
      val r = IRInputs(inputsId, Some(schema), irParams)
      ctx.inputParams.append(r)
      r


case class IROutput(
       id: String,
       schema: Option[Schema],
       expr: DataExpression
) extends IRNode:

  def subnode(names: Seq[String]) =
    if (names.isEmpty) Some(this) else None


object IROutput:

   def create(ctx: IRContext, schema: OutputSchema,  rootPath: String): IROutput =
      val r = IROutput(rootPath, Some(schema), schema.expr)
      ctx.outputs.append(r)
      r



