package net.projecteuler


object Problem63 extends Application {

	def digitsOf(x: BigInt) = x.toString.length

	def log10(x: Double): Double = java.lang.Math.log10(x)
	
	val MAX_N = List.range(1, 100).find((n: Int) => n-1 > n*log10(9)).get - 1

	def solve() = {
	  val solutions = 
	 	  for(n <- 1 until MAX_N+1; y <- 1 until 10; if digitsOf(BigInt(y).pow(n)) == n )
	 	 	  yield (y, n, BigInt(y).pow(n))
	  solutions.size
	}
	
	printf("There are %d n-digit positive integers which are also an nth power?\n", solve)
}