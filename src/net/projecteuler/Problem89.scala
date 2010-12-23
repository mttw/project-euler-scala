package net.projecteuler

import scala.io.Source
import RomanNumeral._

object Problem89 extends Application {
	
  def solve() = {
	  val simpleRomanLetters = Source.fromFile("roman.txt").getLines.toList 
	  val numbers = simpleRomanLetters.map(RomanNumeral.toInt)
	  val romanLetters = numbers.map(RomanNumeral.toRomanNumeral)

	  // calculate savings
	  simpleRomanLetters.map(_.size).reduceLeft(_+_) - romanLetters.map(_.size).reduceLeft(_+_)
  }

  println("The number of characters saved by writing each of these in their minimal form is " + solve)
  
}
