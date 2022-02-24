package day11

object DumboOctopus extends App:

  def loadData(fileName: String): List[List[Int]] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(_.toArray.toList.map(_.asDigit))


  def makeOctopi(octopiMap: List[List[Int]]): Map[Pos, Octopus] = 
    {for {
       y <- 0 until octopiMap.length
       x <- 0 until octopiMap(y).length
       v = octopiMap(x)(y)
    } yield (Pos(x,y) -> Octopus(v, false))}.toMap

  def getLegalNeighbours(p: Pos, m: Map[Pos, Octopus]): Map[Pos, Octopus] =

    Map.empty

case class Octopus(v: Int, flashing: Boolean)

case class Pos(x: Int, y: Int) {
    def upLeft = Pos(x - 1, y + 1)
    def up = Pos(x, y + 1)
    def upRight =  Pos(x + 1, y + 1)
    def left = Pos(x - 1, y)
    def right = Pos(x + 1, y)
    def downLeft = Pos(x - 1, y - 1) 
    def down = Pos(x, y - 1)
    def downRight = Pos(x + 1, y - 1)
    def neighbours(bounds: Pos) = List(upLeft, up, upRight, left, right, downLeft, down, downRight)
      .filter(p => (p.x >= 0 && p.y >= 0 && p.x <= bounds.x && p.y <= bounds.y)) 
}

