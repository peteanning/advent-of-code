package day12

object PassagePaths extends App:

  val start = Vertex("start")

  def loadData(fileName: String): List[(String, String)] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(line => line.split('-')).map(line => (line(0), line(1)))

  def addLegalNeighboursII(findNeighboursFor: Vertex, // A
          pathForVertex: List[Vertex], // start/A/c/A
          adjacencyList: AdjacencyList // the whole graph
          ): List[(Vertex, List[Vertex])] =

    val neighbours = adjacencyList.neighbours(findNeighboursFor)
    // map over the neighbours and remove any that have already been visited
    val unvisited = neighbours.filterNot(n => n.isSmall && pathForVertex.exists(v => n == v))
    unvisited.map(v => (v, pathForVertex :+ v))

  def findPathsII(adjacencyList: AdjacencyList): Int =
    def _findPaths(queue: List[(Vertex, List[Vertex])], completed: Set[List[Vertex]], depth: Int = 0): Int =
      queue match {
        case Nil => completed.size
        case (v, ps) :: vs => {
          val neighbours = addLegalNeighboursII(v, ps, adjacencyList)
          //log(v, ps, neighbours, completed)
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

case class AdjacencyList(lists: List[(String, String)]):
  def neighbours(vertex: Vertex): List[Vertex] =
    lists.map{ (v1, v2) =>
      if v1 == vertex.v then
        Some(Vertex(v2))
      else if v2 == vertex.v then
        Some(Vertex(v1))
      else
        None
    }.filterNot(_ == None).map(m => m.get).sorted


case class Vertex(v: String):
  val isBig = v.forall(_.isUpper)
  val isEnd = v == "end"
  val isStart = v == "start"
  val isSmall = v.forall(_.isLower)

object Vertex:
  given orderingByPoint[A <: Vertex]: Ordering[Vertex] = Ordering.by (v => v.v)
