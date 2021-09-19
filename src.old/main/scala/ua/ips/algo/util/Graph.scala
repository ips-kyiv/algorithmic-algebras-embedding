package ua.ips.algo.util

import scala.collection.mutable.{HashMap => MutableHashMap}

class Graph[NodeId, NodeData, EdgeData]  {

    def get(from: NodeId, to: NodeId): Option[EdgeData] =
      carrier.get(from) match
        case Some(x) => x.get(to)
        case None => None

    def set(from: NodeId, to: NodeId,  value: EdgeData): Unit =
      carrier.get(from) match
        case Some(entry) => entry.put(to,value)
        case None => carrier.put(from, MutableHashMap(to -> value))
      
    def unset(from: NodeId, to: NodeId): Unit =
      carrier.get(from) match
        case Some(entry) => entry.remove(to)
        case None => 

    def removeNode(id: NodeId) =
      carrier.remove(id)
      for ((k,v) <- carrier) {
         v.remove(id)
      }

    

    def childs(from: NodeId): Set[NodeId] =
      carrier.get(from) match
        case Some(entries) => entries.keys.toSet
        case None => Set.empty

        

    private val carrier: MutableHashMap[NodeId,MutableHashMap[NodeId, EdgeData]] = new MutableHashMap()

}