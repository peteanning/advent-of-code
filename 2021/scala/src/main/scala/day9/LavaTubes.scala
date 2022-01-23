package day9

object LavaTubes extends App:

  def walkMap(m: List[List[Int]]) =
    for {
      i <- 0 until m.length
      j <- 0 until m(i).length
      v = m(i)(j)
      if lowest(i, j, m)
    } yield (i, j, v)

  def lowest(i: Int, j: Int, m: List[List[Int]]):Boolean = true
