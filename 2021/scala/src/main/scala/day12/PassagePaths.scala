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
    def _findPaths(queue: List[Vertex], currentPath: List[Vertex], visited: Set[List[Vertex]]): List[List[Vertex]] = 
      queue match {
        case Nil => visited.toList
        case v :: vs => {
          println(s"Queue $queue")
          val _currentPath = currentPath :+ v
          println(s"Current Path $_currentPath")
          println(s"Visited $visited")

          if isComplete(_currentPath) && visited.contains(_currentPath) then
            // base case
            visited.toList
          else if(isComplete(_currentPath)) then
            val _visited = visited + _currentPath
            _findPaths(vs, List.empty, _visited)
          else
            val ns = adjacencyList.get(v.v).getOrElse(List.empty)
            _findPaths(ns ++ vs, _currentPath, visited)
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
