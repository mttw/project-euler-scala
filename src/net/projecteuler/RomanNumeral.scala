package net.projecteuler

object RomanNumeral {

	private val romanValues = Array[Tuple2[String, Int]](
			("M", 1000), ("CM", 900), ("D", 500), ("CD", 400),
			("C", 100), ("XC", 90), ("L", 50), ("XL", 40), ("X", 10), ("IX", 9),
			("V", 5), ("IV", 4), ("I", 1))
			
			
    def toRomanNumeral(num: Int): String = { 
		
		var roman = ""
		var N = num

		for ((letter, value) <- romanValues) {
			while (N >= value) {
				roman += letter
				N -= value
			}
		}
		return roman
	}    	
			
    def toInt(letters: String): Int = {

		val roman = letters.toUpperCase

		var i = 0 // A position in the string, roman
		var arabic = 0 // Arabic numeral equivalent of the part of the string
		// that has
		// been converted so far.

		while (i < roman.length()) {

			val number = letterToNumber(roman(i))

			i+=1 // Move on to next position in the string

			if (i == roman.length()) {
				// There is no letter in the string following the one we have
				// just processed.
				// So just add the number corresponding to the single letter to
				// arabic.
				arabic += number
			} else {
				// Look at the next letter in the string. If it has a larger Roman numeral
				// equivalent than number, then the two letters are counted together as
				// a Roman numeral with value (nextNumber - number).
				val nextNumber = letterToNumber(roman(i))
				if (nextNumber > number) {
					// Combine the two letters to get one value, and move on to
					// next position in string.
					arabic += (nextNumber - number)
					i+=1
				} else {
					// Don't combine the letters. Just add the value of the one
					// letter onto the number.
					arabic += number
				}
			}
		} // end while

		return arabic;
	}
	
  private val ROMAN_VALUE = Map[Char, Int](
		  'I' -> 1,
		  'V' -> 5,
		  'X' -> 10,
		  'L' -> 50,
		  'C' -> 100,
		  'D' -> 500,
		  'M' -> 1000)
	
  private def letterToNumber(letter: Char) = ROMAN_VALUE.get(letter) match {
	  case o: Some[Int] => o.get
	  case _ => throw new NumberFormatException("")
  }
	
	
	
}