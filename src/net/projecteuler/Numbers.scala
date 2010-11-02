package net.projecteuler


object Numbers {
  def bigints(n: BigInt): Stream[BigInt] = Stream.cons(n, bigints(n+1))
  def ints(n: Int): Stream[Int] = Stream.cons(n, ints(n+1))
}

object Partition {
  
  /**
   * Stream of Partitions of n beginning from 1.
   * 
   * Algorithm from: http://www.numericana.com/answer/numbers.htm#partitions
   */
def partitions(): Stream[BigInt] = {
    def partitionsTR(i: Int, P: Map[Int, BigInt]): Stream[BigInt] = { 
      var (j, k, s) = (1, 1, BigInt(0))
      while(j > 0) {
        j = i - (3*k*k + k)/2
        if(j >= 0) s -= BigInt(-1).pow(k)*P(j)
        
        j = i - (3*k*k - k)/2
        if(j >= 0) s -= BigInt(-1).pow(k)*P(j)
        
        k += 1
      }
      Stream.cons(s, partitionsTR(i+1, P + (i -> s)))
    }
    partitionsTR(1, Map[Int, BigInt](0 -> BigInt(1)))
  }    
  /**
   * Partition Function.
   * 
   * See http://en.wikipedia.org/wiki/Integer_partition on Integer Partitions.
   *  
   * @param m The number to be partitioned
   * @return The number of paritions of <code>m</code>
   */
  def partition(n: Int): BigInt = if(n <= 0) 0 else partitions.apply(n-1)  
}

object Calculus {
  def sum(xs: Seq[Int]): Int = (0 /: xs) (_ + _)
  def sum(xs: Seq[BigInt]): BigInt = (BigInt(0) /: xs) (_ + _)

  def sumOfDigits(x: BigInt) = 
	  x.toString.map((x: Char) => BigInt(x.toString)).foldLeft(BigInt(0))(_ + _)

  def factorial(n: BigInt): BigInt = 
	  if(n > 1) n * factorial(n-1) 
	  else if(n == 1) 1 
	  else throw new IllegalArgumentException("Factorial argument must be > 0, is " + n);
	  
}

object Sieve {

  

//  private def primeStream(nums: Stream[BigInt]): Stream[BigInt] =
//    Stream.cons(nums.head, primeStream ((nums tail) filter (x => x % nums.head != 0)) )

  def primeStream: Stream[BigInt] = {
    def primeStream1(nums: Stream[BigInt]): Stream[BigInt] =
      Stream.cons(nums.head, primeStream1 ((nums tail) filter (x => x % nums.head != 0)) )

    primeStream1(Numbers.bigints(2))
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


