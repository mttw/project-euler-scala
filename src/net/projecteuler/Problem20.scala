package net.projecteuler

import Calculus.{sumOfDigits, factorial}

object Problem20 extends Application {

	implicit def pimp(i:Int) = new { def ! = factorial(i) }

	val n = 100
	printf("The sum of the digits in the number %d! is %d\n", n, sumOfDigits(n!))
}