package day12

object PassagePaths extends App:

  val start = Vertex("start")
  
  type AdjacencyList = Map[String, List[Vertex]]
  type Path = List[Vertex]
  type VistedBy = Vertex

  def loadData(fileName: String): List[(String, String)] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(line => line.split('-')).map { line =>
      if line(1) == "start" || line(0) == "end" then
        (line(1), line(0))
      else
        (line(0), line(1))
                              }

  def makeAdjacencyList(data: List[(String, String)]): Map[String, List[Vertex]] =
    (data.groupMap((m,n) => m)((x,y) => Vertex(y))).mapValues(l => l.sorted).toMap

  def addPathsForLargeCaves(al: AdjacencyList): AdjacencyList =
      val largeCaves: List[(String, List[(Vertex, List[Vertex])])] = al.filter((k,v) => Vertex(k).isBig)
        .mapValues(vs => vs.filterNot(_.isEnd)).mapValues(vs => vs.map(v => (v, al.get(v.v).getOrElse(List.empty)))).toList
      val alWithReturnPaths: AdjacencyList = largeCaves.flatMap((k, l) => l.map((v, vs) => (v.v, vs :+ Vertex(k)))).toMap
      al ++ alWithReturnPaths

  def addLegalNeighbours(findNeighboursFor: Vertex, // A
          pathForVertex: List[Vertex], // start/A/c/A
          adjacencyList: AdjacencyList // the whole graph
          ): List[(Vertex, List[Vertex])] =

    val neighbours = adjacencyList.get(findNeighboursFor.v).getOrElse(List.empty)
    // map over the neighbours and remove any that have already been visited
    val unvisited = neighbours.filterNot(n => n.isSmall && pathForVertex.exists(v => n == v))
    unvisited.map(v => (v, pathForVertex :+ v))


  def findPaths(adjacencyList: AdjacencyList): Int =

    def _findPaths(queue: List[(Vertex, List[Vertex])], completed: Set[List[Vertex]], depth: Int = 0): Int =
      queue match {
        case Nil => println(s"Completed paths: ${completed.toList.map(l => l.map(_.v).mkString("/"))}"); completed.size
        case (v, ps) :: vs => {
          val neighbours = addLegalNeighbours(v, ps, adjacencyList)
          log(v, ps, neighbours, completed)
          if v.isEnd then
            _findPaths(vs,completed + ps, depth +1)
          else
            _findPaths(vs ++ neighbours, completed, depth +1)
        }
      }

    _findPaths(List((start, List(start))), Set.empty)

  def log(v:Vertex, ps: List[Vertex], neighbours: List[(Vertex, List[Vertex])], completed: Set[List[Vertex]]): Unit =
       println(s"Popping $v with a path of ${ps.map(_.v).mkString("/")}")
       println(s"Current Path  $ps")
       println(s"There are ${completed.size} completed paths")
       println(s"${v.v} has these neighbours ${neighbours.map((v,_) => v)}")

  def isComplete(p: Path): Boolean =
    p.exists(_.v == "end")


case class Vertex(v: String):
  val isBig = v.forall(_.isUpper)
  val isEnd = v == "end"
  val isStart = v == "start"
  val isSmall = v.size == 1 && v.charAt(0).isLower

object Vertex:
  given orderingByPoint[A <: Vertex]: Ordering[Vertex] = Ordering.by (v => v.v)
