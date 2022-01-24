package day9
import LavaTubes._

class LavaTubesSpec extends org.scalatest.funsuite.AnyFunSuite {

  def m: List[List[Int]] = List(List(1,2,3), List(4,5,6), List(7,8,0))

  def d = loadData("/day9/test.txt")

  test("Walk the Map") {
    assert(walkMap(m) == List( Height(Pos(0,0),1), Height(Pos(2,2),0)))    
  }

  test("loadData") {
    assert(d.length == 5)
    assert(d(0).length == 10)
    assert(d(0)(0) == 2)
    assert(d(4)(9) == 8)

  }

  test("loadData and findLowest co-ordinates") {
    assert(walkMap(d) == List(Height(Pos(0,1),1), Height(Pos(0,9),0), Height(Pos(2,2),5), Height(Pos(4,6),5)))
  }

  test("The Sum of the heights plus one at the lowest points") {
    assert(riskLevels(walkMap(d)) == 15)
  }

  test("getLegalNeighbours for a basin should return all members of the basin") {
    val result = getLegalNeighbours(Height(Pos(0,0), 2), d)
    assert(result.length == 3)
  }

  test("discover all the basins") {
    val result = discoverBasins(walkMap(d), d)
    assert(result.length == 4)
  }

  test("calculate the product of the sizes of the 3 largest basins") {
    val result = productOfThreeLargestBasinSizes(discoverBasins(walkMap(d), d))
    assert(result == 1134)
  }
  
}
