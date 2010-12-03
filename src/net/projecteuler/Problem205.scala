package net.projecteuler

import scala.collection.mutable.HashMap

object Problem205 extends Application {
	def cartesian(sets: List[List[Int]]): List[List[Int]] = 
		(for(set <- sets) yield(for(entry <- set) yield List(entry)))
			.reduceLeft((s0,s1)=>s0.flatMap(a=>s1.map(a ::: _)))

	def cartesianPow(set: List[Int], exp: Int) = 
		cartesian((1 until exp + 1).map((n: Int) => set).toList)
	
	def probabilities(dice: List[Int], numOfDice: Int) = {
		val combos = cartesianPow(dice, numOfDice)
		
		val map = HashMap[Int, BigInt]()
		for(value <- combos.map(_.sum)) {
		  map += value -> (map.getOrElse(value, BigInt(0))+1)
		}
		val numOfValues = combos.size
		(for((k,v) <- map) yield (k, Rational(v, numOfValues))).toMap
	}

	def sum(l: List[Rational]) = l.foldLeft(Rational(0))(_ + _)
	
	def probOfDiceThrowLessThan(n: Int, probs: Map[Int, Rational]) = 
		sum(probs.filter(tuple => tuple._1 < n).values.toList)
	
	val petesProbs =  probabilities(List(1,2,3,4), 9)
	val colinsProbs = probabilities(List(1,2,3,4,5,6), 6)

	val result = sum((for((petesThrow, petesProb) <- petesProbs) 
		yield petesProb*probOfDiceThrowLessThan(petesThrow, colinsProbs)).toList)
		
	printf("Pete wins with a probability of %s, which is approx. %.7f\n", result, result.toDouble)
}
