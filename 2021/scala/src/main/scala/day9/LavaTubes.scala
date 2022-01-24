package day9


object LavaTubes extends App:
 
  def up(p: Pos, m: List[List[Int]]): (Int, Int, Boolean) = (p.i - 1, p.j, (p.i - 1 < 0))
  def down(p: Pos, m: List[List[Int]]): (Int, Int, Boolean)= (p.i + 1, p.j, (p.i+1 > m.length -1))
  def left(p: Pos, m: List[List[Int]]): (Int, Int, Boolean)= (p.i, p.j - 1, (p.j-1 < 0))
  def right(p: Pos, m: List[List[Int]]): (Int, Int, Boolean) = (p.i, p.j + 1, (p.j+1 > m(p.i).length -1))


  def walkMap(m: List[List[Int]]): List[Height] =
    {for {
      i <- 0 until m.length
      j <- 0 until m(i).length
      v = m(i)(j)
      p = Pos(i, j)
      if lowest(p, m)
    } yield Height(p, v)}.toList

  def getLegalNeighbours(h: Height, m: List[List[Int]]) =
    def _getLegalNeighbours(q: List[Height], visited: Set[Height]): List[Height] =
      q match {
        case Nil => visited.toList
        case h :: hs => {
          if !visited.contains(h) then
              val inboundNeighbours = List(up(h.p,m), down(h.p,m), left(h.p,m), right(h.p,m)).filterNot(_._3)
              val heights = inboundNeighbours.map((i,j,b) => Height(Pos(i, j), m(i)(j))).filter(_.height < 9)
              // add the newly found neighbours to the visited list
              val newVisited = visited + h
              _getLegalNeighbours(heights ++ hs ,newVisited) 
          else
            _getLegalNeighbours(hs, visited)
        }
      }
    _getLegalNeighbours(List(h), Set.empty)

  def discoverBasins(lowPoints: List[Height], m: List[List[Int]]): List[List[Height]] =
    lowPoints.map(getLegalNeighbours(_, m))

  def productOdThreeLargestBasinSizes(basins: List[List[Height]]): Int =
    assert(basins.length >= 3, s"There were not enough basis. Found ${basins.length} basins")
    basins.map(_.length).sorted.reverse.take(3).product

  def lowest(p: Pos, m: List[List[Int]]):Boolean = 
    def default(d: (Int, Int, Boolean)): Int =
      if !d._3 then m(d._1)(d._2) else 10

    //println(s"Up $up Down $down Left $left Right $right")

    val h = m(p.i)(p.j)
    val upHeight = default(up(p,m))
    val downHeight = default(down(p,m))
    val leftHeigh = default(left(p,m))
    val rightHeigh = default(right(p,m))

    h < upHeight && h < downHeight && h < leftHeigh && h < rightHeigh

  def riskLevels(heights: List[Height]): Int =
    heights.map(_.height + 1).sum

  def loadData(fileName: String): List[List[Int]] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(_.toArray.toList.map(_.asDigit))

  case class Pos(i: Int, j: Int)
  case class Height(p: Pos, height: Int)
