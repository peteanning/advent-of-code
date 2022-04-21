package day12

object PassagePaths extends App:

  val start = Vertex("start")
  
  type AdjacencyList = Map[String, List[Vertex]]
  type Path = List[Vertex]
  type VistedBy = Vertex

  def loadData(fileName: String): List[(String, String)] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(line => line.split('-')).map(line => (line(0), line(1)))

  def makeAdjacencyList(data: List[(String, String)]): Map[String, List[Vertex]] =
    (data.groupMap((m,n) => m)((x,y) => Vertex(y))).mapValues(l => l.sorted).toMap

  def addPathsForLargeCaves(al: AdjacencyList): AdjacencyList =
      val largeCaves: List[(String, List[(Vertex, List[Vertex])])] = al.filter((k,v) => Vertex(k).isBig)
        .mapValues(vs => vs.filterNot(_.isEnd)).mapValues(vs => vs.map(v => (v, al.get(v.v).getOrElse(List.empty)))).toList
      val alWithReturnPaths: AdjacencyList = largeCaves.flatMap((k, l) => l.map((v, vs) => (v.v, vs :+ Vertex(k)))).toMap
      al ++ alWithReturnPaths
 
  def addLegalNeighbours(v: Vertex, path: List[Vertex], adjacencyList: AdjacencyList, visited: Set[(VistedBy, Vertex)]): List[(Vertex, List[Vertex])] = 
      val neighbours: List[Vertex] = adjacencyList.get(v.v).getOrElse(List.empty)
      neighbours.filterNot(n => visited.contains((path.last, n))).map(n => (n, path :+n))


  def foo(findNeighboursFor: Vertex, // A
          pathForVertex: List[Vertex], // start/A/c/A
          adjacencyList: AdjacencyList, // the whole graph
          visited: Set[(VistedBy, Vertex)] // will contain (A, c)
          ): List[(Vertex, List[Vertex])] =

            val neighbours = adjacencyList.get(findNeighboursFor.v).getOrElse(List.empty)
            // map over the neighbours and remove any that have already been visited
            val unvisited = neighbours.filterNot(n => visited.contains((findNeighboursFor, n)))
            unvisited.map(v => (v, pathForVertex :+ v))


  def findPathsII(adjacencyList: AdjacencyList): Int =

    def _findPaths(queue: List[(Vertex, List[Vertex])], visited: Set[(VistedBy, Vertex)], completed: Set[List[Vertex]]): Int =
      queue match {
        case Nil => println(completed.toList); completed.size
        case (v, ps) :: vs => {
          //println(s"Popping $v with a path of ${ps.map(_.v).mkString("/")}")
          val visitedBy = if ps.size >= 2 then ps(ps.size -2) else v
          //println(s"Visited By $visitedBy")
          val _visited =  
            if v.isBig then 
              visited  
            else
           // println(s"Adding ${(visitedBy, v)} to the visited list")
              visited + ((visitedBy, v)) // we only keep track of small caves during this path 
          val neighbours = addLegalNeighbours(v, ps, adjacencyList, visited)
          //println(s"${v.v} has these neighbours ${neighbours.map((v,_) => v)}") 
          val path = ps :+v
          if completed.contains(path) then
            completed.size
          else if v.isEnd then
            _findPaths(vs, _visited, completed + path)
          else
            _findPaths(neighbours ++ vs, _visited ,  completed)
        }
      }

    _findPaths(List((start, List(start))), Set.empty, Set.empty)


  def findPaths(adjacencyList: Map[String, List[Vertex]]) =
    def _findPaths(queue: List[Vertex], currentPath: List[Vertex], visited: Set[List[Vertex]]): List[List[Vertex]] = 
      queue match {
        case Nil => visited.toList
        case v :: vs => {
          val ns = adjacencyList.get(v.v).getOrElse(List.empty)
          //println(s" Is end ${v.isEnd}")
          //println(s"Queue $queue")
          val _currentPath = currentPath :+ v
          //println(s"Current Path $_currentPath")
          //println(s"Visited $visited")

          if isComplete(_currentPath) && visited.contains(_currentPath) then
            // base case
            println("base case")
            visited.toList
          else if(isComplete(_currentPath)) then
            _findPaths(vs, List.empty, visited + _currentPath)
          else
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
