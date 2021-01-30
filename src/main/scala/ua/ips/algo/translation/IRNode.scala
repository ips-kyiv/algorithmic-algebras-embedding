package ua.ips.algo.translation

import ua.ips.algo._

/**
* schema with additional
**/
sealed trait IRNode{
  def  id: String;
  def  schema: Option[Schema];
  def  costEstimations(ctx: IRContext): CostEstimations;

  def  subnode(ids: Seq[String]): Option[IRNode]

  //def  cfNext(ctx: IRContex): Set[IRNode];
  //def  cfPrev(ctx: IRContex): Set[IRNode];
  //def  isVar: Boolean;
}


object IRNode:

    def accept(ctx: IRContext, schema: Schema, rootPath: String): IRNode =
      schema match
        case s@SequentialSchema(x, y) =>
          SeqIRNode.create(ctx, s, rootPath)
        case p@ParallelSchema(x,y) =>
          ParIRNode.create(ctx,p, rootPath)

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

    def costEstimations(ctx: IRContext): CostEstimations =
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
        case SequentialSchema(x,y) =>
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

   def costEstimations(ctx: IRContext): CostEstimations =
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
      case ParallelSchema(x,y) =>
          append(ctx, append(ctx, prev, x), y)
      case other => 
           val inner = IRNode.accept(ctx, schema, s"${prev.rootPath}.${prev.flowIndex}")
           prev.copy(nodes = prev.nodes :+ inner, flowIndex = prev.flowIndex + 1)

end ParIRNode


case class EmptyIRNode(id: String) extends IRNode:

  def schema: Option[Schema] = None

  def subnode(names: Seq[String]) =
    if (names.isEmpty) Some(this) else None

  def costEstimations(ctx: IRContext) = CostEstimations.ZERO


