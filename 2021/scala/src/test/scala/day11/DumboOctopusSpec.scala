package day11

import DumboOctopus._

class DumboOctopusSpec extends org.scalatest.funsuite.AnyFunSuite {

  def data: List[List[Int]] = loadData("/day11/test.txt")
  def line1: List[Int] = "5483143223".toArray.map(_.asDigit).toList
  def line10: List[Int] = "5283751526".toArray.map(_.asDigit).toList

  def smallData: List[List[Int]] = loadData("/day11/small-test.txt")

  def ZERO: Pos = Pos(0,0)


  test("shoudl load the test data") {
    assert(data.length == 10)
    assert(data(0) == line1)
    assert(data(9) == line10)
  }

  test("small test data set") {
    assert(smallData.length == 5)
  }
  test("make Octopi test") {
    val octopi = makeOctopi(smallData)
    assert(octopi.size == 25)
    assert(octopi.get(ZERO).get == Octopus(1))
    assert(octopi.get(Pos(4,4)).get == Octopus(1))
  }
  test("increment") {
    val octopi = makeOctopi(smallData)
    val i = increment(octopi)
    assert(i.get(ZERO).get == Octopus(2))
    assert(i.get(Pos(4,4)).get == Octopus(2))
  }
  test("bounds") {
    val b = bounds(makeOctopi(smallData))
    assert(b == Pos(4,4))
  }

  test("a  Pos at 0,0 only has 3 neighbours with bound 2,2") {
   val neighbours: List[Pos] = ZERO.neighbours(Pos(2,2)) 
   val expected: List[Pos] = List(Pos(0,1), Pos(1,0), Pos(1,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }
  test("a  Pos at 2,2 only has 3 neighbours with bound  2,2") {
   val neighbours: List[Pos] = Pos(2,2).neighbours(Pos(2,2)) 
   val expected: List[Pos] = List(Pos(1,2), Pos(1,1), Pos(2,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }

  test("a  Pos at 0,2 only has 3 neighbours with bound  2,2") {
   val neighbours: List[Pos] = Pos(0,2).neighbours(Pos(2,2)) 
   val expected: List[Pos] = List(Pos(1,2), Pos(1,1), Pos(0,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }

  test("a  Pos at 2,0 only has 3 neighbours with bound  2,2") {
   val neighbours: List[Pos] = Pos(2,0).neighbours(Pos(2,2)) 
   val expected: List[Pos] = List(Pos(1,0), Pos(1,1), Pos(2,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }
  test("a  Pos at 1,1  8 neighbours with bound  2,2") {
   val neighbours: List[Pos] = Pos(1,1).neighbours(Pos(2,2)) 
   val expected: List[Pos] = List(Pos(0,2), Pos(1,2), Pos(2,2), Pos(0,1), Pos(0,0), Pos(2,1),
     Pos(1,0), Pos(2,0))
   assert(neighbours.size == 8)
   assert(neighbours.sorted == expected.sorted)
  }



    
}

