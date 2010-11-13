package net.projecteuler

import Calculus.sum

object Problem109 extends Application {
  
  object ScoreType extends Enumeration {
     type ScoreType = Value
     val S, D, T = Value
  }
  import ScoreType._
  
  class DartScore(s: ScoreType, region: Int) extends Ordered[DartScore] {
    def value() = (s.id + 1) * region
    def s(): ScoreType = s
    override def toString() = s.toString + region.toString + "(" + value + ")"
    
    override def compare(that: DartScore): Int = 
      if(value != that.value) value.compare(that.value)
      else s.id.compare(that.s.id)

  }

  object DartScore {
    def apply(s: ScoreType, region: Int) = new DartScore(s, region)

    def possibleScores(scoreValue: Int) = {
      val l1 = if(isDartSingleScore(scoreValue)) DartScore(S, scoreValue) :: Nil else Nil
      val l2 = if(isDartDoubleScore(scoreValue)) DartScore(D, scoreValue/2) :: l1 else l1
      if(isDartTripleScore(scoreValue)) DartScore(T, scoreValue/3) :: l2 else l2
    }
  }

  
  
  def isDartTripleScore(points: Int) = (points % 3 == 0) && (points <= 60)
  def isDartDoubleScore(points: Int) = (points % 2 == 0) && (points <= 40 || points == 50)
  def isDartSingleScore(points: Int) = (points <= 20 || points == 25)
  def isDartScore(points: Int) = isDartSingleScore(points) || isDartDoubleScore(points) || isDartTripleScore(points)

  def dartScoresFromTo(from: Int, to: Int): List[DartScore] = 
    (from until to+1).map(DartScore.possibleScores).foldLeft(List[DartScore]())(_ ++ _)
  def dartScoresFrom(from: Int): List[DartScore] = dartScoresFromTo(from, 60)
  val dartScores: List[DartScore] = dartScoresFrom(1)
  
  def isCheckout(score: Int*) = true // TODO
  
  def allTripleCheckoutsForScore(score: Int): List[List[DartScore]] =
    for(a <- dartScores.filter(_.value <= score-2);
        b <- dartScores.filter(_ >= a).filter(_.value <= score-(a.value+1));
        if(isDartDoubleScore(score-(a.value+b.value)))
       ) yield(List(a,b,DartScore(D, (score-(a.value+b.value))/2)))
  
  def allDoubleCheckoutsForScore(score: Int): List[List[DartScore]] =
    for(a <- dartScores.filter(_.value <= score-1);
        if(isDartDoubleScore(score-a.value))
       ) yield(List(a,DartScore(D, (score-a.value)/2)))
  
  def allSingleCheckoutsForScore(score: Int): List[List[DartScore]] =
    if(isDartDoubleScore(score)) List(List(DartScore(D, score/2))) else Nil

  def allCheckoutsForScore(score: Int) =
    allSingleCheckoutsForScore(score) ++ allDoubleCheckoutsForScore(score) ++ allTripleCheckoutsForScore(score)
  
//  val n = 6
//  allCheckoutsForScore(n) foreach println
//  println(allCheckoutsForScore(n).size)

  val n = 100
  val checkouts = (1 until n).map(allCheckoutsForScore(_).size)
  val solution = sum(checkouts)
  
  printf("There are %d distinct ways a player can checkout with a score less than %d.\n", solution, n)
  
  
}
