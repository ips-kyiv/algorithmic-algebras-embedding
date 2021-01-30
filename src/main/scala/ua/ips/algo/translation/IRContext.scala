package ua.ips.algo.translation

import ua.ips.algo._
import ua.ips.algo.util._

import scala.collection.mutable.{HashMap => MutableHashMap}

type NodeId = String

class IRContext(val target: Target) {

  var rootNode: IRNode = EmptyIRNode("")

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
    
  private val allNodes: MutableHashMap[NodeId, IRNode] = MutableHashMap()
}