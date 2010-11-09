package net.projecteuler


object Problem97 extends Application {
  
  val tenBillion = BigInt(10).pow(10)
  val result = (BigInt(28433) * BigInt(2).modPow(BigInt(7830457), tenBillion) + BigInt(1)) % tenBillion
  
  printf("The last ten digits of prime number 28433 * 2^7830457 + 1 are %s.\n", result)
  
  
}
