package net.projecteuler


import scala.collection.immutable.NumericRange
import Numbers.{bigints, isqrt, isSquare}
import Calculus.{toDigitList, digitsToBigInt}

object Problem206 {

	def padDigits(vs: Seq[Int], len: Int) = 
		(for(i <- 0 until (len - vs.size)) yield 0) ++ vs
	def zeroTail(n: BigInt, len: Int) = (n/BigInt(10).pow(len))*BigInt(10).pow(len)	
	def fix2(n: BigInt): BigInt = {
					
		val others = for((d, i) <- toDigitList(n).zipWithIndex; if(i%2==1)) yield d
		val template = Vector(1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,0)
		val nDigits = toDigitList(n)
		if(template.size != nDigits.size) throw new IllegalArgumentException("Invalid input size " + nDigits.size)
		
		var i = 0
		var b = BigInt(0)
		var maxDiff = 0
		for((nd, td) <- nDigits.reverse.zip(template.reverse)) {
		  if(i%2 == 0 && nd != td) maxDiff = i/2 
		  if(i%2 == 0 && nd > td) b += BigInt(10).pow((i/2))
		  i += 1
		}
		
		val fixedOthers = padDigits(toDigitList(zeroTail(digitsToBigInt(others) + b, maxDiff)), template.size/2)
		if(others.size != fixedOthers.size) 
			throw new IllegalArgumentException("No result for give input, size exceeded: " + fixedOthers.size)
		
		val fillers = Vector(1,2,3,4,5,6,7,8,9,0)
		val vs = (for((df, d) <- fillers.zipAll(fixedOthers, 0, 0)) yield df.toString + d.toString).toList.reduceLeft(_+_)
		val fixedN = BigInt(vs)/10
		fixedN
	}
	
    def solve(): BigInt = {
    	var n = BigInt(10).pow(9)
		while(true) {
		  val nsq = n*n
		  val fixed = fix2(nsq)
		  if(isSquare(fixed)) return isqrt(fixed)
		  n = if(n*n > nsq) isqrt(fixed) else isqrt(nsq)+1
		}
    	throw new IllegalArgumentException("unreachable")
    }
	
    def main(args: Array[String]) {
    	printf("The unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0 is %d\n", solve)
    }
}
