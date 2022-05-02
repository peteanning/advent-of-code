package day12

object PassagePaths extends App:

  val start = Vertex("start")

  def loadData(fileName: String): List[(String, String)] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(line => line.split('-')).map(line => (line(0), line(1)))

  def addLegalNeighbours(findNeighboursFor: Vertex, // A
          pathForVertex: List[Vertex], // start/A/c/A
          adjacencyList: AdjacencyList // the whole graph
          ): List[(Vertex, List[Vertex])] =


    val neighbours = adjacencyList.neighbours(findNeighboursFor)
    // map over the neighbours and remove any that have already been visited
    val unvisited = neighbours.filterNot(n => (n.isSmall || n.isStart) && pathForVertex.exists(v => n == v))
    unvisited.map(v => (v, pathForVertex :+ v))

  def addLegalNeighboursII(findNeighboursFor: Vertex, // A
          pathForVertex: List[Vertex], // start/A/c/A
          adjacencyList: AdjacencyList, // the whole graph
          maybeSmall: Option[Vertex] = None
          ): List[(Vertex, List[Vertex])] =

    val neighbours = adjacencyList.neighbours(findNeighboursFor)
    val unvisited = maybeSmall.fold {
      neighbours.filterNot(n => (n.isSmall || n.isStart) && pathForVertex.exists(v => n == v))
      }{ v =>
        neighbours.filterNot{ n =>
          if n.isSmall && n == v then
           pathForVertex.count(p => p == v) >= 2
          else
            (n.isSmall || n.isStart) && pathForVertex.exists(v => n == v)
        }
      }
    // map over the neighbours and remove any that have already been visited
    //val unvisited = neighbours.filterNot(n => (n.isSmall || n.isStart) && pathForVertex.exists(v => n == v))
    unvisited.map(v => (v, pathForVertex :+ v))


  def findPathsPart2(adjacencyList: AdjacencyList): Int =

    def _findPaths(queue: List[(Vertex, List[Vertex])], completed: Set[List[Vertex]], small: Option[Vertex]): Set[List[Vertex]] =
      queue match {
        case Nil => completed
        case (v, ps) :: vs => {
          val neighbours = addLegalNeighboursII(v, ps, adjacencyList, small)
          //log(v, ps, neighbours, completed)
          if v.isEnd then
            _findPaths(vs,completed + ps, small)
          else
            _findPaths(neighbours ++ vs, completed, small)
        }
      }

    def vistSmallTwice(smalls: List[Vertex], acc: Set[List[Vertex]]): Int =
      smalls match {
        case Nil => acc.size
        case s :: ss => {
          val _acc =  _findPaths(List((start, List(start))), Set.empty, Some(s))
          vistSmallTwice(ss, acc ++ _acc)
        }
      }
    vistSmallTwice(adjacencyList.smalls, Set.empty)


  def logCompleted(completed: Set[List[Vertex]]) =
    completed.foreach{
      l =>
        println(l.map(_.v).mkString(","))
    }



  def findPaths(adjacencyList: AdjacencyList): Int =

    def _findPaths(queue: List[(Vertex, List[Vertex])], completed: Set[List[Vertex]]): Int =
      queue match {
        case Nil => completed.size
        case (v, ps) :: vs => {
          val neighbours = addLegalNeighbours(v, ps, adjacencyList)
          //log(v, ps, neighbours, completed)
          if v.isEnd then
            _findPaths(vs,completed + ps)
          else
            _findPaths(vs ++ neighbours, completed)
        }
      }

    _findPaths(List((start, List(start))), Set.empty)

  def log(v:Vertex, ps: List[Vertex], neighbours: List[(Vertex, List[Vertex])], completed: Set[List[Vertex]]): Unit =
       println(s"Popping $v with a path of ${ps.map(_.v).mkString("/")}")
       //println(s"Current Path  $ps")
      // println(s"There are ${completed.size} completed paths")
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
  val smalls = lists.foldRight(Set.empty[Vertex])
      ((v, acc) => (acc + Vertex(v._1)) + Vertex(v._2)).filter(_.isSmall).toList


case class Vertex(v: String):
  val isBig = v.forall(_.isUpper)
  val isEnd = v == "end"
  val isStart = v == "start"
  val isSmall = v.forall(_.isLower) && !isStart && !isEnd

object Vertex:
  given orderingByPoint[A <: Vertex]: Ordering[Vertex] = Ordering.by (v => v.v)

