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

  def addLegalNeighbours(findNeighboursFor: Vertex, // A
          pathForVertex: List[Vertex], // start/A/c/A
          adjacencyList: AdjacencyList, // the whole graph
          visited: Set[(List[Vertex], Vertex)] // will contain (A, c)
          ): List[(Vertex, List[Vertex])] =

    val neighbours = adjacencyList.get(findNeighboursFor.v).getOrElse(List.empty)
    println(s"Find neighbours for ${findNeighboursFor.v} pathForVertex ${pathForVertex.map(_.v).mkString("/")} visited ${visited.toList}")
    // map over the neighbours and remove any that have already been visited
    val unvisited = neighbours.filterNot(n => n.isSmall && pathForVertex.exists(v => n == v))
    unvisited.map(v => (v, pathForVertex :+ v))


  def findPaths(adjacencyList: AdjacencyList): Int =

    def _findPaths(queue: List[(Vertex, List[Vertex])], visited: Set[(List[Vertex], Vertex)], completed: Set[List[Vertex]], depth: Int = 0): Int =
      queue match {
        case Nil => println(s"Completed paths: ${completed.toList.map(l => l.map(_.v).mkString("/"))}"); completed.size
        case (v, ps) :: vs => {
          println(s"Popping $v with a path of ${ps.map(_.v).mkString("/")}")
          println(s"Current Path  $ps")
          println(s"There are ${completed.size} completed paths")
          val _visited =  if v.isSmall then visited + ((ps, v)) else visited // we only keep track of small caves during this path 
          println(s"visted Set is now $_visited")
          val neighbours = addLegalNeighbours(v, ps, adjacencyList, _visited)
          println(s"${v.v} has these neighbours ${neighbours.map((v,_) => v)}") 
          if v.isEnd then
            _findPaths(vs, _visited, completed + ps, depth +1)
          else
            _findPaths(vs ++ neighbours, _visited ,  completed, depth +1)
        }
      }

    _findPaths(List((start, List(start))), Set.empty, Set.empty)


  def isComplete(p: Path): Boolean =
    p.exists(_.v == "end")


case class Vertex(v: String):
  val isBig = v.size == 1 && v.charAt(0).isUpper
  val isEnd = v == "end"
  val isStart = v == "start"
  val isSmall = v.size == 1 && v.charAt(0).isLower

object Vertex:
  given orderingByPoint[A <: Vertex]: Ordering[Vertex] = Ordering.by (v => v.v)
