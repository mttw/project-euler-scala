package net.projecteuler

import Calculus.digits
import scala.collection.immutable.NumericRange
import Numbers.bigints
import Numbers.{isqrt, isSquare}

object Problem206 extends Application {
	val pattern = "1_2_3_4_5_6_7_8_9_0"
    val revPattern = pattern.reverse
    
//    def digits(x: BigInt) = x.toString.map((x: Char) => BigInt(x.toString))
    def digits(x: BigInt) = x.toString.map((x: Char) => x.toString.toInt)
    
	def generateFromPattern(): Seq[BigInt] = {
	    def apply(n: BigInt) = {
			var revp = revPattern
			for(d <- digits(n).reverse) revp = revp.replaceFirst("_", d.toString)
			revp = revp.replaceAll("_", "0")
			BigInt(revp.reverse)
		}
//	    for(n <- NumericRange.inclusive(BigInt(10).pow(9), BigInt(10).pow(10), BigInt(1))) continue
	    for(n <- bigints(0, BigInt(10).pow(7))) yield n
	}
	
	def find(n: BigInt): BigInt = BigInt(isqrt(n).bigInteger.toString) + 1
	def matchesPattern(n: BigInt) = 
		Vector(1,2,3,4,5,6,7,8,9,0) == (for((c,i) <- n.toString.zipWithIndex; if(i%2==0)) yield c.toString.toInt)

	def fix(n: BigInt): BigInt = {
		val vn = n.toString.map(_.toString.toInt)
		var arr = Array(1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,0)
		if(arr.size != vn.size) throw new IllegalArgumentException("WAAAAA " + vn.size)
		
		for((c,i) <- vn.zipWithIndex; if(i%2 == 1)) { 
			if(vn(i+1)>arr(i+1) && c < 9) arr.update(i, c+1) 
			else if(vn(i+1)>arr(i+1) && c == 9) { 
			  if(i>1) { arr.update(i, 0); arr.update(i-2, arr(i-2)+1) }
			  else throw new IllegalArgumentException("WAAAAA keine weiteren Lösungen")
			}
			else arr.update(i, c)
		}
		return BigInt(arr.map(_.toString).reduceLeft(_+_))
	}
	def digitsToBigInt(vs: Seq[Int]) = 
		BigInt(vs.map(_.toString).foldLeft("")(_+_))

	def padDigits(vs: Seq[Int], len: Int) = 
		(for(i <- 0 until (len - vs.size)) yield 0) ++ vs
	def zeroTail(n: BigInt, len: Int) = (n/BigInt(10).pow(len))*BigInt(10).pow(len)	
	def fix2(n: BigInt): BigInt = {
					
		val others = for((d, i) <- digits(n).zipWithIndex; if(i%2==1)) yield d
		val template = Vector(1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,0)
		val nDigits = digits(n)
//		println(template)
//		println(nDigits)
		if(template.size != nDigits.size) throw new IllegalArgumentException("WAAAAA " + nDigits.size)
		
		var i = 0
		var b = BigInt(0)
//		println(template)
//		println(nDigits)
		var maxDiff = 0
		for((nd, td) <- nDigits.reverse.zip(template.reverse)) {
		  if(i%2 == 0 && nd != td) maxDiff = i/2 
		  if(i%2 == 0 && nd > td) {
		 	  b += BigInt(10).pow((i/2))
		 	  
		  }
		  i += 1
		}
		
		val fixedOthers = padDigits(digits(zeroTail(digitsToBigInt(others) + b, maxDiff)), template.size/2)
//		println(digitsToBigInt(others))
//		println(b)
//		println(fixedOthers)
		if(others.size != fixedOthers.size) throw new IllegalArgumentException("WAAAAA2 " + fixedOthers.size)
		
		val fillers = Vector(1,2,3,4,5,6,7,8,9,0)
		val vs = (for((df, d) <- fillers.zipAll(fixedOthers, 0, 0)) yield df.toString + d.toString).toList.reduceLeft(_+_)
		val fixedN = BigInt(vs)/10
//		println(n)
//		println(fixedN)
		fixedN
	}
	
	println(zeroTail(123456,4))
//	var n2 = n*n
//	val t = BigInt("1020304050607080900")
//	val t2 = BigInt("1030304050607080900")
//	println(matchesPattern(t))
//	println(matchesPattern(t2))
//	println(t)
//	println(fix(t))
//	println(t2)
//	println(fix(t2))
//	val x = find(t)
//	println(x)
//	println((x).pow(2))
	

//	val b = BigInt("1020394979696160900")
	val b = BigInt("1020394979696160901")
//	val b = BigInt("1930304979696160900")
	println(b)
	println(fix2(b))
//	System.exit(0)
	var n = BigInt(10).pow(9)
	val o = BigInt(10).pow(7)
	println("Start")
//	for(i<- 0 until 1000) {
	while(true) {
	  val nsq = n*n
	  println(nsq)
	  val fixed = fix2(nsq)
	  println(fixed)
	  println
	  if(isSquare(fixed)) {
	 	  println(fixed)
	 	  println("********")
	 	  println(isqrt(fixed))
	 	  System.exit(0)
	  }
	  n = isqrt(fixed)
	  if(n*n <= nsq) n = isqrt(nsq)+1
	}
	
//	while(true) {
//	for(i<- 0 until 100) {
//		val n2 = n*n
//		println(n2)
//		println(matchesPattern(n2))
//		if(!matchesPattern(n2)) {
//			val x = fix(n2)
//			println(x + " (fixed)")
//			val est = (find(x)/10)*10
//			println(x + " (est)")
//			if(est == n) n += 10 else n = est
//		} else {
//			println("HIT")
//			System.exit(0)
//			n += 10
//		}
//		
////		n += 10
//	}
	
//	println(generateFromPattern.last)
//	bigints(0, 10).foreach(println)
//	printf("Pete wins with a probability of %s, which is approx. %.7f\n", result, result.toDouble)
}
