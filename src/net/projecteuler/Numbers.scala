package net.projecteuler

object Calculus {
  def sum(xs: Seq[Int]): Int = (0 /: xs) (_ + _)

  def sumOfDigits(x: BigInt) = 
	  x.toString.map((x: Char) => BigInt(x.toString)).foldLeft(BigInt(0))(_ + _)

  def factorial(n: BigInt): BigInt = 
	  if(n > 1) n * factorial(n-1) 
	  else if(n == 1) 1 
	  else throw new IllegalArgumentException("Factorial argument must be > 0, is " + n);
	  
}

object Sieve {

  private def bigints(n: BigInt): Stream[BigInt] = Stream.cons(n, bigints(n+1))

//  private def primeStream(nums: Stream[BigInt]): Stream[BigInt] =
//    Stream.cons(nums.head, primeStream ((nums tail) filter (x => x % nums.head != 0)) )

  def primeStream: Stream[BigInt] = {
    def primeStream1(nums: Stream[BigInt]): Stream[BigInt] =
      Stream.cons(nums.head, primeStream1 ((nums tail) filter (x => x % nums.head != 0)) )

    primeStream1(bigints(2))
  }

  def primes(n: Int) = primeStream take n toList
}


object Fibonacci {
	
  def fib(n: Int) = {
    def fibTR(n: Int, b: Int, a: Int): Int = n match {
      case 0 => a 
      case _ => fibTR(n - 1, a + b, b)
    }
	fibTR(n, 1, 0) 
  }

  def fibList(limit: Int): List[Int] = {
	  def fibListTR(limit: Int, xs: List[Int]): List[Int] = {
	 	  // TODO wurm.matthias: Find solution to access last 2 elements (so the final reverse can be removed)
	 	  val a :: b :: _ = xs
	 	  if(a+b > limit) xs else fibListTR(limit, a+b :: xs)
	  }
	  fibListTR(limit, List(1, 0)).reverse
  }
  
}


