package day9
import LavaTubes._

class LavaTubesSpec extends org.scalatest.funsuite.AnyFunSuite {

  def m: List[List[Int]] = List(List(1,2,3), List(4,5,6), List(7,8,0))

  def d = loadData("/day9/test.txt")

  test("Walk the Map") {
    assert(walkMap(m) == List((0,0,1), (2,2,0)))    
  }

  test("loadData") {
    assert(d.length == 5)
    assert(d(0).length == 10)
    assert(d(0)(0) == 2)
    assert(d(4)(9) == 8)

  }

  test("loadData and findLowest co-ordinates") {
    assert(walkMap(d) == List((0,1,1), (0,9,0), (2,2,5), (4,6,5)))
  }

  test("The Sum of the heights plus one at the lowest points") {
    assert(riskLevels(walkMap(d)) == 15)
  }
  
}
