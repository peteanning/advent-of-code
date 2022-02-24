package day11

import DumboOctopus._

class DumboOctopusSpec extends org.scalatest.funsuite.AnyFunSuite {

  def data: List[List[Int]] = loadData("/day11/test.txt")
  def line1: List[Int] = "5483143223".toArray.map(_.asDigit).toList
  def line10: List[Int] = "5283751526".toArray.map(_.asDigit).toList

  def smallData: List[List[Int]] = loadData("/day11/small-test.txt")

  test("shoudl load the test data") {
    assert(data.length == 10)
    assert(data(0) == line1)
    assert(data(9) == line10)
  }

  test("small test data set") {
    assert(smallData.length == 5)
  }
  test("make Octopi test") {
    val octopi: Map[Pos, Octopus] = makeOctopi(smallData)
    assert(octopi.size == 25)
    assert(octopi(Pos(0,0)) == Octopus(1, false))
    assert(octopi(Pos(4,4)) == Octopus(1, false))
  }
  test("Pos objects and their valid neighbours") {
    
    val bounds = Pos(4,4)

    val p = Pos(1,1)
    val neighbours = p.neighbours(bounds)
    assert(neighbours.length == 8)
    assert(neighbours.contains(p.up))
    assert(p.up == Pos(1,2))

    assert(neighbours.contains(p.left))
    assert(p.left == Pos(0,1))

    assert(neighbours.contains(p.right))
    assert(p.right == Pos(2,1))

    assert(neighbours.contains(p.down))
    assert(p.down == Pos(1,0))

    assert(neighbours.contains(p.upLeft))
    assert(p.upLeft == Pos(0,2))

    assert(neighbours.contains(p.upRight))
    assert(p.upRight == Pos(2,2))

    assert(neighbours.contains(p.downRight))
    assert(p.downRight == Pos(2,0))

    assert(neighbours.contains(p.downLeft))
    assert(p.downLeft == Pos(0,0))


    val bottomLeft = Pos(0,0)
    val neighbours2 = bottomLeft.neighbours(bounds)
    assert(neighbours2.length == 3)

    val topRight = Pos(4,4)
    val neighbours3 = topRight.neighbours(bounds)
    assert(neighbours3.length == 3)

    val bottomRight = Pos(4,0)
    val neighbours4 = bottomRight.neighbours(bounds)
    assert(neighbours4.length == 3)

    val topLeft = Pos(4,4)
    val neighbours5 = topLeft.neighbours(bounds)
    assert(neighbours5.length == 3)

    val topMiddle = Pos(2,4)
    val neighbours6 = topMiddle.neighbours(bounds)
  
    assert(neighbours6.length == 5)
  }
}

