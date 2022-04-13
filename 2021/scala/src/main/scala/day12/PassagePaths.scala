package day12

object PassagePaths extends App:
  
  type AdjacencyList = Map[String, List[Vertex]]
  type Path = List[Vertex]
  type VistedBy = Vertex

  def loadData(fileName: String): List[(String, String)] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(line => line.split('-')).map(line => (line(0), line(1)))

  def makeAdjacencyList(data: List[(String, String)]): Map[String, List[Vertex]] =
    (data.groupMap((m,n) => m)((x,y) => Vertex(y))).mapValues(l => l.sorted).toMap

  def addPathsForLargeCaves(al: AdjacencyList) =
      val largeCaves = al.filter((k,v) => Vertex(k).isBig)
        .mapValues(vs => vs.filter(v => al.get(v.v) == None && v.isEnd == false )).toMap.toList
      val foo = largeCaves.foldLeft(Map[String, List[Vertex]]())
              ((B,A) => B ++ A._2.map(v => (v.v, List(Vertex(A._1)))).toMap)
      al ++ foo

  def findPaths(adjacencyList: Map[String, List[Vertex]]) =
    def _findPaths(queue: List[Vertex], paths: List[Vertex], visited: Set[Vertex]): List[Vertex] = 
      queue match {
        case Nil => visited.toList
        case v :: vs => {
          if visited.contains(v) then
            _findPaths(vs, paths :+ v, visited)
          else
            val maybeNs: Option[List[Vertex]] = adjacencyList.get(v.v)
            maybeNs.fold(_findPaths(vs, paths, visited + v))
               (ns => _findPaths(vs ++ ns, paths :+ v, visited + v)) 
        }
      }
  

    _findPaths(List(Vertex("start")), List.empty, Set.empty)

  def isComplete(p: Path): Boolean =
    p.exists(_.v == "end")


case class Vertex(v: String):
  val isBig = v.size == 1 && v.charAt(0).isUpper
  val isEnd = v == "end"
  val isStart = v == "start"

object Vertex:
  given orderingByPoint[A <: Vertex]: Ordering[Vertex] = Ordering.by (v => v.v)
