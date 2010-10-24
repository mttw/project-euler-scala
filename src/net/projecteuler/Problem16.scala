package net.projecteuler

import Calculus.sumOfDigits

object Problem16 extends Application {
	
	val exp = 1000
	val s = sumOfDigits(BigInt(2).pow(exp))
	printf("The sum of the digits of 2^%d is %s\n", exp, s)
	
}