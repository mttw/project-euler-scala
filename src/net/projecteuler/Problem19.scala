package net.projecteuler
import Calculus.sum
import java.util.GregorianCalendar
import java.util.Calendar


object Problem19 extends Application {

	def getFirstDayInMonth(year: Int, month: Int) = 
		new GregorianCalendar(year, month-1, 1).get(Calendar.DAY_OF_WEEK) 

	def firstDayInMonthIsASunday(year: Int, month: Int) = 
		getFirstDayInMonth(year, month) == Calendar.SUNDAY 

	def solve(fromYear: Int, toYear: Int) = {
	  val monthsStartingWithASunday = 
	 	  for(y <- fromYear until toYear+1; m <- 1 until 13; if(firstDayInMonthIsASunday(y, m))) 
	 	 	    yield (y, m)
	  monthsStartingWithASunday.size
	}
	
	val (startYear, endYear) = (1901, 2000)
	printf("The number of sundays on the first of the month from %d-01-01 to %d-12-31 is %d\n", 
			startYear, endYear, solve(startYear, endYear))
}