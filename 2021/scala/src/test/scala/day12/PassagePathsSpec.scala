package day12

import PassagePaths._

class PassagePathsSpec extends org.scalatest.funsuite.AnyFunSuite {

  def data: List[(String, String)] = loadData("/day12/test.txt")
  def paths(fileName: String): List[List[Vertex]] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close
    text.map(line => line.split(',').toList.map(v => Vertex(v)))
  def vertice =  paths("/day12/test-paths.txt")


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
}
