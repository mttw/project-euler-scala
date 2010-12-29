package net.projecteuler

import Partition.partitions
import scala.io.Source
import Calculus.toDigitList

object Problem79 {
  
  val keylog = List(319, 680, 180, 690, 129, 620, 762, 689, 762, 318, 368, 710, 720, 710, 629, 168, 160, 689, 716, 731, 736, 729, 316, 729, 729, 710, 769, 290, 719, 680, 318, 389, 162, 289, 162, 718, 729, 319, 790, 680, 890, 362, 319, 760, 316, 729, 380, 319, 728, 716)
	
  def toEdges(logins: Int*): Set[Edge[Int]] = toEdges(logins.toIterable)

  def toEdges(logins: Iterable[Int]): Set[Edge[Int]] = {
    val loginDigits = logins.map(toDigitList(_))
	val edges = for(login <- loginDigits; (a,b) <- login.zip(login.tail)) yield Edge(a,b)
    edges.toSet
  }

  def isValidLogin(pin: Int, passcode: Int): Boolean = {
	  val passcodeDigits = toDigitList(passcode)
	  toDigitList(pin).forall(d => passcodeDigits.contains(d))
  }
	
  def main(args: Array[String]) = {
	val g = DirectedGraph(toEdges(keylog))
	
	println("Edges:   " + g.edges)
	println("Sources: " + g.sources)
	println("Sinks:   " + g.sinks)
	
	// by having a look at the graph, following solutions can be seen
	val solution = 73162890 
	println(solution)
	assert(keylog.forall(isValidLogin(_, solution)) == true)
	
  }
  
}
