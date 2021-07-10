package ua.ips.algo.translation

import ua.ips.algo._
import ua.ips.algo.util._

import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.{Seq => MutableSeq}
import scala.collection.mutable.ArrayBuffer


type NodeId = String

class IRContext(val target: Target, val fullName: Seq[String]) {

  val rootId = "root"

  var rootNode: IRNode = EmptyIRNode(rootId)

  val inputParams: ArrayBuffer[IRInputs] = ArrayBuffer[IRInputs]()
  val outputs: ArrayBuffer[IROutput] = ArrayBuffer[IROutput]()

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
    println(s"IRCOntext.put $id  $node")
    allNodes.put(id, node)
    if (id == rootId) then
      rootNode = node;
    
  private val allNodes: MutableHashMap[NodeId, IRNode] = MutableHashMap()
}

