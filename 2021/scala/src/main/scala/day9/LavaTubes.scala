package day9

object LavaTubes extends App:

  def walkMap(m: List[List[Int]]): List[(Int, Int, Int)] =
    {for {
      i <- 0 until m.length
      j <- 0 until m(i).length
      v = m(i)(j)
      if lowest(i, j, m)
    } yield (i, j, v)}.toList

  def lowest(i: Int, j: Int, m: List[List[Int]]):Boolean = 
    def default(d: (Int, Int, Boolean)): Int =
      if !d._3 then m(d._1)(d._2) else 10

    val up: (Int, Int, Boolean) = (i - 1, j, (i - 1 < 0))
    val down: (Int, Int, Boolean)= (i + 1, j, (i+1 > m.length -1))
    val left: (Int, Int, Boolean)= (i, j - 1, (j-1 < 0))
    val right: (Int, Int, Boolean) = (i, j + 1, (j+1 > m(i).length -1))

    //println(s"Up $up Down $down Left $left Right $right")

    val h = m(i)(j)
    val upHeight = default(up)
    val downHeight = default(down)
    val leftHeigh = default(left)
    val rightHeigh = default(right)

    h < upHeight && h < downHeight && h < leftHeigh && h < rightHeigh

  def riskLevels(heights: List[(Int, Int, Int)]): Int =
    heights.map(_._3 + 1).sum

  def loadData(fileName: String): List[List[Int]] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(_.toArray.toList.map(_.asDigit))

    
