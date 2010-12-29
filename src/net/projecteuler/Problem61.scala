package net.projecteuler

import PolygonalNumbers._
import Calculus.sum
object Problem61 {

  def isConnected(x: BigInt, y: BigInt) = (x % 100) == (y / 100)

  def solve(xss: List[List[BigInt]], len: Int): List[List[BigInt]] = {
    def s(path: List[BigInt], yss: List[List[BigInt]]): List[List[BigInt]] = {
      if(path.size == len) {
        if(isConnected(path.last, path.head)) return List(path)
        else return Nil
      }

      return (for(ys <- yss; next <- ys.filter(isConnected(path.last, _)))
          yield s(path ::: List(next), yss - ys)).flatten.toList
    }

    (for(x <- xss.head) yield s(List(x), xss - xss.head)).flatten.toList
  }


  def main(args: Array[String]) = {
    val len = 6
    val xss = (for(i <- 3 until 3+len) yield polygonals(i).takeWhile(_ < 10000).filter(_ >= 1000).toList).toList
    val solutions = solve(xss, len)
    println(solutions.head)
    println(sum(solutions.head))
  }

}