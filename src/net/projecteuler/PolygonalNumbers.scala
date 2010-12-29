package net.projecteuler


object PolygonalNumbers {

  def polygonals(i: Int) = {
    def p(n: BigInt): Stream[BigInt] = Stream.cons((n*(n*(i - 2) + 4 - i))/2, p(n+1))
    p(1)
  }
  def triangulars() = polygonals(3)

  def squares(): Stream[BigInt] = polygonals(4)

  def pentagonals(): Stream[BigInt] = polygonals(5)

  def hexagonals(): Stream[BigInt] = polygonals(6)

  def heptagonals(): Stream[BigInt] = polygonals(7)

  def octagonals(): Stream[BigInt] = polygonals(8)


}
