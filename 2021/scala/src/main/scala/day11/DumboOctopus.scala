package day11

object DumboOctopus extends App:

  type VisitedBy = Point
  def loadData(fileName: String): List[List[Int]] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(_.toArray.toList.map(_.asDigit))

  def makeOctopi(octopiMap: List[List[Int]]): Map[Point, Octopus] = 
    {for {
       y <- 0 until octopiMap.length
       x <- 0 until octopiMap(y).length
       v = octopiMap(x)(y)
    } yield (Point(x,y) -> Octopus(v))}.toMap

  def increment(octopiMap: Map[Point,Octopus]) =
    octopiMap.mapValues(_.increment).toMap
    
  def bounds(octopiMap: Map[Point, Octopus]) =
    octopiMap.keys.maxBy[(Int, Int)](p => (p.x, p.y))

  def step(octopiMap: Map[Point, Octopus]) =
    val b = bounds(octopiMap)
    val incremented = increment(octopiMap)
    val flashers = incremented.filter((_,o) => o.flashing).toList.map((p,o) => p)

    def _step(flashingQueue: List[Point], os: Map[Point, Octopus], visited: Set[(VisitedBy, Point)]): Map[Point, Octopus] =
      flashingQueue match {
        case Nil => os
        case p :: ps => {

          val neighboursNotVisited : List[(VisitedBy, Point)] = 
            (visited -- p.neighbours(b).map(point => (p,point)).toSet).toList
          val neighboursInc = neighboursNotVisited.map((_,p) => (p, os(p).increment))
          val updatedOctupi = os ++ neighboursInc.toMap 
          val flashers = neighboursInc.filter((p,o) => o.flashing).map((p,_) => p)
          val newVisited = visited ++ p.neighbours(b).map(point => (p, point))
          _step(flashers ++ flashingQueue, updatedOctupi, newVisited)
        }
      }
    
    _step(flashers, incremented, Set.empty)

case class Octopus(v: Int) {
  def flashing: Boolean = v > 9
  def increment = new Octopus(v + 1)
}

case class Point(x: Int, y: Int) {
    def upLeft = Point(x - 1, y + 1)
    def up = Point(x, y + 1)
    def upRight =  Point(x + 1, y + 1)
    def left = Point(x - 1, y)
    def right = Point(x + 1, y)
    def downLeft = Point(x - 1, y - 1) 
    def down = Point(x, y - 1)
    def downRight = Point(x + 1, y - 1)
    def neighbours(bounds: Point) = List(upLeft, up, upRight, left, right, downLeft, down, downRight)
      .filter(p => (p.x >= 0 && p.y >= 0 && p.x <= bounds.x && p.y <= bounds.y)) 
}

object Point:
  // ordering converts a Point into a tuple and uses the builtin ordering for that
  given orderingByPoint[A <: Point]: Ordering[Point] = Ordering.by (p => (p.x, p.y))


