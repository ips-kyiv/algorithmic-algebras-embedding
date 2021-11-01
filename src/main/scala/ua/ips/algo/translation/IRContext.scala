package ua.ips.algo.translation

import ua.ips.algo._
import ua.ips.algo.util._

import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.{Seq => MutableSeq}
import scala.collection.mutable.ArrayBuffer


type NodeId = String

/**
 *
 **/
class IRContext(val target: Target, 
                val fullName: Seq[String], 
                val variant: Seq[String],
                var rootNode: IRNode = EmptyIRNode(IRContext.rootId),
                val inputParams: ArrayBuffer[IRInputs] = ArrayBuffer[IRInputs](),
                val outputs: ArrayBuffer[IROutput] = ArrayBuffer[IROutput](),
                val allNodes: MutableHashMap[NodeId, IRNode] = MutableHashMap()
                ) {


  def nodeById(id:String): IRNode =
    allNodes.get(id) match
      case Some(node) => node
      case None =>
        rootNode.subnode(id.split('.').toSeq) match
          case Some(node) =>
            addNode(id, node)
            node
          case None => 
            throw new RuntimeException(s"Node with id ${id} is not found in context");
          
  val controlFlow:Graph[NodeId, Boolean, Boolean] = Graph()

  def addNode(id: NodeId, node: IRNode): Unit =
    allNodes.put(id, node)
    if (id ==IRContext.rootId) then
      rootNode = node;

  def removeNode(id: NodeId): Unit =
    if (false) {
        // check for correctness.
    }
    allNodes.remove(id)

  def  fork(variants: Seq[String]): Seq[IRContext] = 
    variants.map( newVariant => 
        IRContext(target, fullName, variants.appended(newVariant),
           rootNode, inputParams.clone(), outputs.clone(),
           allNodes.clone()
        )
    )

  
}

object IRContext {
  val rootId = "root";
}

