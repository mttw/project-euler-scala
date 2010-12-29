package net.projecteuler

/**
 * Created by IntelliJ IDEA.
 * User: wurm.matthias
 * Date: 25.12.10
 * Time: 13:21
 * To change this template use File | Settings | File Templates.
 */

object Combination {
  def cartesian(sets: List[List[Int]]): List[List[Int]] =
    (for(set <- sets) yield(for(entry <- set) yield List(entry)))
      .reduceLeft((s0,s1)=>s0.flatMap(a=>s1.map(a ::: _)))


  def choose2[T](set: Seq[T]): Seq[Tuple2[T, T]] =
    for((a, i) <- set.zipWithIndex; (b, j) <- set.zipWithIndex; if(i != j))
      yield (a, b)

  def choose2Different[T](set: Seq[T]): Seq[Tuple2[T, T]] =
    for((a, i) <- set.zipWithIndex; (b, j) <- set.zipWithIndex; if(j > i))
      yield (a, b)


//  def choose(n: Int, xs: Seq[Int]): Seq[Seq[Int]] = {
//    def c(chosen: List[Int], ys: List[Int]): Seq[Seq[Int]] = {
//       if(chosen.length == n) List(chosen)
//       else (for(y <- ys) yield(c(y :: chosen, ys - y))).flatten.toList
//    }
//
//    c(Nil, xs.toList)
//  }

  def choose[T](n: Int, xs: Seq[T]): Seq[Seq[T]] = {
    def c(chosen: List[T], ys: List[T]): Seq[Seq[T]] = {
       if(chosen.length == n) List(chosen)
       else (for(y <- ys) yield(c(y :: chosen, ys - y))).flatten.toList
    }

    c(Nil, xs.toList)
  }
  
}