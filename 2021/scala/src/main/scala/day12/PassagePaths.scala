package day12

object PassagePaths extends App:

  def loadData(fileName: String): List[(String, String)] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(line => line.split('-')).map(line => (line(0), line(1)))

  def makeAdjacencyList(data: List[(String, String)]): Map[String, List[Vertex]] =
    (data.groupMap((m,n) => m)((x,y) => Vertex(y))).mapValues(l => l.sorted).toMap


case class Vertex(v: String):
  def isBig = v.size == 0 && v.charAt(0).isUpper

object Vertex:
  given orderingByPoint[A <: Vertex]: Ordering[Vertex] = Ordering.by (v => v.v)
