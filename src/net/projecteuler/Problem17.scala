package net.projecteuler
import Calculus.sum

object Problem17 extends Application {
	
  val dict = Map(
	BigInt(0) -> "zero",
	BigInt(1) -> "one",
	BigInt(2) -> "two",
	BigInt(3) -> "three",
	BigInt(4) -> "four",
	BigInt(5) -> "five",
	BigInt(6) -> "six",
	BigInt(7) -> "seven",
	BigInt(8) -> "eight",
	BigInt(9) -> "nine",
	BigInt(10) -> "ten",
	BigInt(11) -> "eleven",
	BigInt(12) -> "twelve",
	BigInt(13) -> "thirteen",
	BigInt(14) -> "fourteen",
	BigInt(15) -> "fifteen",
	BigInt(16) -> "sixteen",
	BigInt(17) -> "seventeen",
	BigInt(18) -> "eighteen",
	BigInt(19) -> "nineteen",
	BigInt(20) -> "twenty",
	BigInt(30) -> "thirty",
	BigInt(40) -> "forty",
	BigInt(50) -> "fifty",
	BigInt(60) -> "sixty",
	BigInt(70) -> "seventy",
	BigInt(80) -> "eighty",
	BigInt(90) -> "ninety"
  )

  val dict2 = Map(
    BigInt(100) -> "hundred",
	BigInt(1000) -> "thousand"
  )

  
  def numOfDigits(x: BigInt) = x.toString.length
  
  def countLetters(s: String) = s.replace(" ", "").size
  
  def say(x: BigInt): String = {
	  def say100(x: BigInt): String = dict.get(x) match {
		  case Some(s) => s
		  case None => val (a, b) = x /% 10; dict(a*10) + (if(b.intValue != 0) " " + dict(b)) 
	  }
	  
	if(numOfDigits(x) <= 2) {
		say100(x)
	} else {
		val (firstDigit, remainder) = x /% BigInt(10).pow(numOfDigits(x) - 1)
		val firstPart = dict(firstDigit) + " " + dict2(BigInt(10).pow(numOfDigits(x) - 1))
		val secondPart = 
			if(remainder > 0 && remainder < 100) " and " + say100(remainder) 
			else if (remainder > 0) " " + say(remainder) 
			else ""
		firstPart + secondPart
	}
  }
  
  def solve(n: Int) = {
	  val numbersInWords = for(i <- List.range(1,n+1)) yield say(i)
	  sum(numbersInWords.map(countLetters(_)))
  }
  
  val n = 1000
  printf("If all the numbers from 1 to %d were written out in words, %d letters would be used\n", n, solve(n))
}