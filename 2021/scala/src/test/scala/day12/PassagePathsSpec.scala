package day12

import PassagePaths._

class PassagePathsSpec extends org.scalatest.funsuite.AnyFunSuite {

  def data: List[(String, String)] = loadData("/day12/test.txt")
  def dataTest2: List[(String, String)] = loadData("/day12/test-2.txt")
  def dataTest3: List[(String, String)] = loadData("/day12/test-3.txt")
  def loadPaths(fileName: String): List[List[Vertex]] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close
    text.map(line => line.split(',').toList.map(v => Vertex(v)))
  def vertice =  loadPaths("/day12/test-paths.txt")

  test ("Vertex should have properties") {
    val v = Vertex("c")
    val vBig = Vertex("C")
    val start = Vertex("start")
    val end = Vertex("end")

    assert(v.isBig == false)
    assert(vBig.isBig == true)
    
    assert(end.isEnd == true)
    assert(end.isBig == false)
    assert(end.isSmall == false)
    
    assert(start.isStart == true)
    assert(start.isBig == false)
    assert(start.isSmall == false)


    val HG = Vertex("HG")
    assert(HG.isBig)
    assert(HG.isSmall == false)

    val dc = Vertex("dc")
    assert(dc.isSmall)
  }

  test("load data") {
    assert(("start", "A") == data(0))
    assert(("b", "end") == data(6))
  }

  test ("should get all the small Vertex in a graph") {
    val smalls = AdjacencyList(data).smalls.sorted
    val expected = List(Vertex("b"), Vertex("c"), Vertex("d")).sorted
    assert(expected == smalls)

  }

  test ("should load test 2 data") {
    assert(("dc", "end") == dataTest2(0))
    assert(("HN", "start") == dataTest2(1))
    assert(("start", "kj") == dataTest2(2))
    assert(("dc", "start") == dataTest2(3))
    assert(("dc", "HN") == dataTest2(4))
    assert(("LN", "dc") == dataTest2(5))
    assert(("HN", "end") == dataTest2(6))
    assert(("kj", "sa") == dataTest2(7))
    assert(("kj", "HN") == dataTest2(8))
    assert(("kj", "dc") == dataTest2(9))
  }

  test ("should make case class AdjacencyList") {
    val adjacencyList = AdjacencyList(dataTest2)
    val neighbours = adjacencyList.neighbours(Vertex("start"))
    val expected = List(Vertex("HN"), Vertex("kj"), Vertex("dc")).sorted
    assert(expected == neighbours)
  }

  test ("should load Vertex") {
    assert(vertice(0) == List("start","A","b","A","c","A","end").map(Vertex(_)))
    assert(vertice(9) == List("start","b","end").map(Vertex(_)))
  }  


  test ("should visit small caves at most once in a path to the end") {
      val al = AdjacencyList(data)
      val findNeighboursForA = Vertex("A")
      val pathForVertex = List(Vertex("start"), Vertex("A"), Vertex("c"), Vertex("A"))
      val result = addLegalNeighbours(findNeighboursForA, pathForVertex, al).sorted
      val expected = List((Vertex("end"), pathForVertex :+ Vertex("end")),
                          (Vertex("b"), pathForVertex :+ Vertex("b"))).sorted
      assert(expected == result)
  }

  test ("should visit small caves at most twice in a path to the end") {
      val al = AdjacencyList(data)
      val findNeighboursForA = Vertex("d")
      val pathForVertex = List(Vertex("start"), Vertex("A"), Vertex("b"), Vertex("d"))
      val result = addLegalNeighboursII(findNeighboursForA, pathForVertex, al, Some(Vertex("b"))).sorted
      val expected = List((Vertex("b"), pathForVertex :+ Vertex("b"))).sorted
      assert(expected == result)
  }



  test ("Graph should be cyclic") {
    val al = AdjacencyList(data)
    val neighboursA = al.neighbours(Vertex("A"))
    val expectedA = List(Vertex("end"), Vertex("c"), Vertex("b"), Vertex("start")).sorted
    assert(expectedA == neighboursA)

    val expected_c = List(Vertex("A"))
    val neighbours_c = al.neighbours(Vertex("c"))
    assert(expected_c == neighbours_c)
  }

  test ("should findPaths test 1") {
    val al = AdjacencyList(data)
    val paths = findPaths(al)
    assert(paths == 10)
  }

  test ("should findPaths in test 2") {
    val al = AdjacencyList(dataTest2)
    val paths = findPaths(al)
    assert(paths == 19)

  }

  test ("should findPaths in test 3") {
    val al = AdjacencyList(dataTest3)
    val paths = findPaths(al)
    assert(paths == 226)
  }

  test ("should findPaths test 1 Part2") {
    val al = AdjacencyList(data)
    val paths = findPathsPart2(al)
    assert(paths == 36)
  }

  test ("should findPaths in test 2 Part2") {
    val al = AdjacencyList(dataTest2)
    val paths = findPathsPart2(al)
    assert(paths == 103)

  }

  test ("should findPaths in test 3 Part2") {
    val al = AdjacencyList(dataTest3)
    val paths = findPathsPart2(al)
    assert(paths == 3509)
  }


}
