package day12

import PassagePaths._

class PassagePathsSpec extends org.scalatest.funsuite.AnyFunSuite {

  def data: List[(String, String)] = loadData("/day12/test.txt")
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
    

  }

  test("load data") {
    assert(("start", "A") == data(0))
    assert(("b", "end") == data(6))
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
  test ("should findPaths") {
    val _data = loadData("/day12/test-small.txt")
    val al = addPathsForLargeCaves(makeAdjacencyList(_data))
    val paths = findPaths(al)
    println(paths.size)
  }
  test ("should find Vertex that are connected to by large caves") {
    val testData = loadData("/day12/test-2.txt")
    val al = makeAdjacencyList(testData)
    val result = addPathsForLargeCaves(al)
    val v = result("x")
    val c = result("c")
    assert(v == List(Vertex("A")))
    assert(c == List(Vertex("A")))

  }
}
