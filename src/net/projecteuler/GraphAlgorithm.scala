package net.projecteuler

import scala.collection.mutable.ListBuffer
import Ordering.fromLessThan

object Graph {
  def apply[T](edges: Set[Edge[T]]) = {
	  val vertices = edges.map(e => Set(e.vertices._1, e.vertices._2)).foldLeft(Set[Vertex[T]]())(_ union _)
	  new Graph(vertices, edges)
  }
}

// unidirected
class Graph[T](v: Set[Vertex[T]], e: Set[Edge[T]]) {
  val vertices = v
  val edges = e

  private val neighborMap = buildNeighborMap
  
  def N(v: Vertex[T]) = neighborMap(v)
  
  def isNeighbor(v1: Vertex[T], v2: Vertex[T]) = N(v1).contains(v2)
  
  
  private def buildNeighborMap(): Map[Vertex[T], Set[Vertex[T]]] = {
    def findNeighbors(v: Vertex[T]) = {
    	val rightSides = edges.filter(e => e.vertices._1 == v).map(_.vertices._2)
    	val leftSides = edges.filter(e => e.vertices._2 == v).map(_.vertices._1)
    	rightSides union leftSides
    }
    vertices.map(v => (v, findNeighbors(v))).toMap
  }
  
}

object DirectedGraph {
  def apply[T](edges: Set[Edge[T]]) = {
	  val vertices = edges.map(e => Set(e.vertices._1, e.vertices._2)).foldLeft(Set[Vertex[T]]())(_ union _)
	  new DirectedGraph(vertices, edges)
  }
}

class DirectedGraph[T](v: Set[Vertex[T]], e: Set[Edge[T]]) {
  val vertices = v
  val edges = e

  private val outgoingVerticesMap =
	vertices.map(v => (v, edges.filter(e => e.vertices._1 == v).map(_.vertices._2))).toMap

  private val incomingVerticesMap =
    vertices.map(v => (v, edges.filter(e => e.vertices._2 == v).map(_.vertices._1))).toMap
  
    
  def incomingVerticesOf(v: Vertex[T]) = incomingVerticesMap(v)

  def outgoingVerticesOf(v: Vertex[T]) = outgoingVerticesMap(v)

  def inDegreeOf(v: Vertex[T]) = incomingVerticesOf(v).size

  def outDegreeOf(v: Vertex[T]) = outgoingVerticesOf(v).size
  
  def sources() = vertices.filter(inDegreeOf(_) == 0)

  def sinks() = vertices.filter(outDegreeOf(_) == 0)

}


object Vertex {
  def apply[T](v: T) = new Vertex(v)
}

class Vertex[T](v: T) {
  val value = v
  
  override def toString() = v.toString
  
  override def equals(obj: Any) = obj match {
    case v: Vertex[T] => value == v.value
    case _ => false
  }
  
  override def hashCode() = value.hashCode
  
}

object Edge {
  def apply[T](v1: Vertex[T], v2: Vertex[T]) = new Edge(v1, v2)
  def apply[T](v1: T, v2: T) = new Edge(Vertex(v1), Vertex(v2))
  def apply[T](t: Tuple2[T, T]) = new Edge(Vertex(t._1), Vertex(t._2))
}

class Edge[T](v1: Vertex[T], v2: Vertex[T]) {
  val vertices = Tuple2(v1, v2)
  
  override def toString() = vertices.toString

  override def equals(obj: Any) = obj match {
    case e: Edge[T] => vertices == e.vertices 
    case _ => false
  }

  override def hashCode() = vertices.hashCode
  
}

object GraphAlgorithm {
  def findAllCliques[T](graph: Graph[T], limit: Int = 2): Seq[Set[Vertex[T]]] = {
	  def toMutableSet[A](t: Tuple2[A,A]) = scala.collection.mutable.Set(t._1, t._2)
	  
	  val start = graph.edges.map(e => toMutableSet(e.vertices))
	  val sets = List(start.toList:_*)
	  var inc = 1
	  while(inc > 0) {
	 	  inc = 0
	 	  for(v <- graph.vertices; set <- sets.filter(!_.contains(v));
	 	   if(set.forall(w => graph.isNeighbor(v, w)))) {
	 	 	  set += v
	 	 	  inc += 1
	 	  }
	  }
	   
	  // make immutable and distinct
	  sets.map(_.toSet).toList.distinct.filter(_.size >= limit)
  }
	
  /**
   * Find maximal clique in a given graph
   */
  def bronKerbosch[T](graph: Graph[T]) = {
	def largestSubset[A](sets: Iterable[Set[A]]): Set[A] = {
		if(sets.size == 0) return Set[A]()
		else {
			val sizeOrdering = fromLessThan[Set[A]](_.size < _.size)
			return sets.max(sizeOrdering)
		}
	}
    def bk1(R: Set[Vertex[T]], P: Set[Vertex[T]], X: Set[Vertex[T]]): Set[Vertex[T]] = {
    	if(P.isEmpty && X.isEmpty) 
    	  return R
    	
    	var P1 = P
    	var X1 = X
    	val lb = ListBuffer[Set[Vertex[T]]]()
    	for(v <- P1) {
    		lb.append(bk1(R + v, P1.intersect(graph.N(v)), X1.intersect(graph.N(v))))
    		P1 = P1 - v
    		X1 = P1 + v
    	}
    	
    	return largestSubset(lb)
    }
    
    bk1(Set(), graph.vertices, Set())
  }



  def edgesToMap[T](edges: Set[Edge[T]]) = {
    edges.map(edge => (edge.vertices._1, edges.filter(e => e.vertices._1 == edge.vertices._1).map(_.vertices._2)))
  }


  def main(args: Array[String]) = {
    
  }
}


