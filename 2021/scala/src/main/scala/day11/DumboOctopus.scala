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
  def reset(octopiMap: Map[Point, Octopus]) = 
    octopiMap.map((p, o) => (p, o.reset))

  def neighboursNotVisited(visitor: Point, visited: Set[(VisitedBy, Point)],
    bounds: Point): List[(VisitedBy,Point)] =
    
    val neighbours = visitor.neighbours(bounds).map(p => (visitor, p)).toSet
    (neighbours -- visited).toList
    
  def flashers(octopiMap: Map[Point, Octopus]) =
    octopiMap.filter((_,o) => o.flashing).toList.map((p,o) => p)

  def update(octopiMap: Map[Point, Octopus], newMap: Map[Point, Octopus]): Map[Point, Octopus] =
    octopiMap ++ newMap

  def step(octopiMap: Map[Point, Octopus], noOfSteps: Int, noOfFlashes: Int): (Map[Point, Octopus], Int) =
    noOfSteps match {
      case 0 => (octopiMap, countFlashes(octopiMap) + noOfFlashes)
      case _ => {
        val os = step(octopiMap)
        val remainingSteps = noOfSteps - 1
        val flashes = countFlashes(os)
        val currentTotalFlashes = noOfFlashes + flashes
        step(reset(os), remainingSteps, currentTotalFlashes)
      }
    }

  def targetStep(octopiMap: Map[Point, Octopus]): Int =
    val limit = 1000
    val noOfOctopi = octopiMap.size
    def _targetStep(octopiMap: Map[Point, Octopus], noOfSteps: Int): Int =
      if noOfSteps < limit then
        val s = step(octopiMap)
        if countFlashes(s) == noOfOctopi then
          noOfSteps + 1
        else
          _targetStep(reset(s), noOfSteps + 1)
      else
        -1
    _targetStep(octopiMap, 0)

  def countFlashes(octopiMap: Map[Point, Octopus]): Int =
    flashers(octopiMap).size

  def step(octopiMap: Map[Point, Octopus]) =
    val b = bounds(octopiMap)
    val incremented = increment(octopiMap)
    val flash = flashers(incremented)

    def _step(flashingQueue: List[Point], os: Map[Point, Octopus], visited: Set[(VisitedBy, Point)]): Map[Point, Octopus] =
      flashingQueue match {
        case Nil => os
        case p :: ps => {
          val notVisited : List[(VisitedBy, Point)] = neighboursNotVisited(p, visited, b)
          val neighboursInc: List[(Point, Octopus)] =
            notVisited.map((_,p) => (p, os(p).increment))
          val updatedOctupi = update(os, neighboursInc.toMap)
          val flashing = flashers(neighboursInc.toMap)
          val newVisited = visited ++ notVisited.toSet
          _step(ps ++ flashing, updatedOctupi, newVisited)
        }
      }
    _step(flash, incremented, Set.empty)

case class Octopus(v: Int) {
  def flashing: Boolean = v > 9
  def increment = new Octopus(v + 1)
  def reset = 
    if flashing then
      new Octopus(0)
    else
      this
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


