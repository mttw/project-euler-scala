package net.projecteuler

object Problem128 extends Application {
  
  def layer(n: Int) = 
    if(n<0) Nil 
    else if(n == 0) List(1) 
    else List.range(3*n*n - 3*n + 2, 3*(n+1)*(n+1) - 3*(n+1) + 2)  
 
  def outsideNeighbours(i: Int, l: Int): List[Int] = {
    if(l == 0) {
      List.range(0,6) 
    }
    else {
      if(i == 0) {
        List(6*(l+1) - 1, 0, 1)
      } 
      else if(i%l == 0) {
        val m = (i/l)*(l+1)
        List(m-1, m, m+1)
      } 
      else {
        val m = (i/l)*(l+1) + 1
        List(m, m+1)
      }
    }
  }

  def insideNeighbours(i: Int, l: Int): List[Int] = {
    if(l <= 0) Nil
    else if(l == 1) List(0)
    else {
      if(i == 6*l - 1) List(i-(i/l+1), 0)
      else if(i%l == 0) List((i/l)*(l-1))
      else { val m = i-(i/l+1) ; List(m, m+1) }
      
    }
  }

  def directNeighbours(i: Int, l: Int): List[Int] = {
    if(l <= 0) Nil
    else {
      if(i == 0) List(6*l - 1, 1)
      else if(i == 6*(l) - 1) List(i-1, 0)
      else List(i-1, i+1)
    }
  }
  

  def neighbours(l: Int): Seq[Tuple2[Int, Seq[Int]]]= {
    val insideElements = layer(l-1)
    val layerElements = layer(l)
    val outsideElements = layer(l+1)
    for(i <- 0 until layerElements.size)
      yield (layerElements(i),
           List.concat(
               for(j <- insideNeighbours(i, l)) yield(insideElements(j)), 
               for(j <- directNeighbours(i, l)) yield(layerElements(j)),
               for(j <- outsideNeighbours(i, l)) yield(outsideElements(j))
               )
           )
  }
  

  def neighbours(): Stream[Tuple2[Int, Seq[Int]]] = {
    def ns1(l: Int): Stream[Tuple2[Int, Seq[Int]]] = {
      val n = neighbours(l)
      Stream.cons(n.head, Stream.concat(n.tail.toStream, ns1(l+1)))
    }
    ns1(0)
  }

//  for(i <- 1 until 7) println(layer(i))
  println((neighbours take 10000).last)
}
