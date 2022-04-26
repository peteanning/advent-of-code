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
    assert(start.isStart == true)
    assert(end.isEnd == true)
    
    assert(start.isBig == false)
    assert(start.isSmall == false) 

    assert(end.isBig == false)
    assert(end.isSmall == false)

    val HG = Vertex("HG")
    assert(HG.isBig)
  }

  test("load data") {
    assert(("start", "A") == data(0))
    assert(("b", "end") == data(6))
  }

  test ("should make paths face away from the start and towards the end") {
    assert(("start", "HN") == dataTest2(1))
    assert(("zg", "end") == dataTest3(5))
  }


  test ("should return an adjacency list from raw data") {
   val al = makeAdjacencyList(data)
   assert(al("start") == List(Vertex("A"), Vertex("b")).sorted)
   assert(al("A") == List(Vertex("c"), Vertex("b"),Vertex( "end")).sorted)
   assert(al("b") == List(Vertex("end"), Vertex("d")).sorted)
  }  

  test ("should load Vertex") {
    assert(vertice(0) == List("start","A","b","A","c","A","end").map(Vertex(_)))
    assert(vertice(9) == List("start","b","end").map(Vertex(_)))
  }  
  test ("should check if a Path is complete") {
   val completePath = List(Vertex("start"), Vertex("end"))
   val inCompletePath = List(Vertex("start"), Vertex("a"))

   assert(isComplete(completePath) == true)
   assert(isComplete(inCompletePath) == false)
  }
  test ("should visit small caves at most once in a path to the end") {
      val al = makeAdjacencyList(data)
      val alWithLargeCaves = addPathsForLargeCaves(al)
      val findNeighboursForA = Vertex("A")
      val pathForVertex = List(Vertex("start"), Vertex("A"), Vertex("c"), Vertex("A"))
      val visited = Set((pathForVertex, Vertex("c")))
      val result = addLegalNeighbours(findNeighboursForA, pathForVertex, alWithLargeCaves).sorted
      val expected = List((Vertex("end"), pathForVertex :+ Vertex("end")),
                          (Vertex("b"), pathForVertex :+ Vertex("b"))).sorted

      assert(expected == result)

  }
  test ("should findPaths") {
    val _data = loadData("/day12/test.txt")
    val al = makeAdjacencyList(_data)
    val alWithLargeCaves = addPathsForLargeCaves(al)
    val paths = findPaths(alWithLargeCaves)
    assert(paths == 10)

  }

  test ("should findPaths in test 2") {
    val al = makeAdjacencyList(dataTest2)
    val alWithLargeCaves = addPathsForLargeCaves(al)
    //val paths = findPaths(alWithLargeCaves)
    //assert(paths == 19)

  }
  test ("should find Vertex that are connected to by large caves") {
    val al = makeAdjacencyList(data)
    val result = addPathsForLargeCaves(al)
    val c = result("c")
    val b = result("b").sorted
    val A = Vertex("A")
    assert(c == List(A))
    assert(b == List(A, Vertex("d"), Vertex("end")).sorted)
    assert(result("start").sorted == List(A, Vertex("b")).sorted)
    assert(result.get("end") == None)
    assert(result("A").sorted == List(Vertex("b"), Vertex("end"), Vertex("c")).sorted)

  }
}
