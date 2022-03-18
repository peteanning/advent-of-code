package day11

import DumboOctopus._

class DumboOctopusSpec extends org.scalatest.funsuite.AnyFunSuite {

  def data: List[List[Int]] = loadData("/day11/test.txt")
  def line1: List[Int] = "5483143223".toArray.map(_.asDigit).toList
  def line10: List[Int] = "5283751526".toArray.map(_.asDigit).toList

  def smallData: List[List[Int]] = loadData("/day11/small-test.txt")

  def ZERO: Point = Point(0,0)

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
    assert(octopi.get(Point(4,4)).get == Octopus(1))
  }
  test("increment") {
    val octopi = makeOctopi(smallData)
    val i = increment(octopi)
    assert(i.get(ZERO).get == Octopus(2))
    assert(i.get(Point(4,4)).get == Octopus(2))
  }
  test("bounds") {
    val b = bounds(makeOctopi(smallData))
    assert(b == Point(4,4))
  }

  test("a  Point at 0,0 only has 3 neighbours with bound 2,2") {
   val neighbours: List[Point] = ZERO.neighbours(Point(2,2)) 
   val expected: List[Point] = List(Point(0,1), Point(1,0), Point(1,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }
  test("a  Point at 2,2 only has 3 neighbours with bound  2,2") {
   val neighbours: List[Point] = Point(2,2).neighbours(Point(2,2)) 
   val expected: List[Point] = List(Point(1,2), Point(1,1), Point(2,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }

  test("a  Point at 0,2 only has 3 neighbours with bound  2,2") {
   val neighbours: List[Point] = Point(0,2).neighbours(Point(2,2)) 
   val expected: List[Point] = List(Point(1,2), Point(1,1), Point(0,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }

  test("a  Point at 2,0 only has 3 neighbours with bound  2,2") {
   val neighbours: List[Point] = Point(2,0).neighbours(Point(2,2)) 
   val expected: List[Point] = List(Point(1,0), Point(1,1), Point(2,1))
   assert(neighbours.size == 3)
   assert(neighbours.sorted == expected.sorted)
  }
  test("a  Point at 1,1  8 neighbours with bound  2,2") {
   val neighbours: List[Point] = Point(1,1).neighbours(Point(2,2)) 
   val expected: List[Point] = List(Point(0,2), Point(1,2), Point(2,2), Point(0,1), Point(0,0), Point(2,1),
     Point(1,0), Point(2,0))
   assert(neighbours.size == 8)
   assert(neighbours.sorted == expected.sorted)
  }

  test("reset the Map after flashing") {
    val inc = increment(increment(makeOctopi(data)))
    assert(inc.find((p,o) => o.flashing) == Some(Point(8,4), Octopus(10)))
    val rst = reset(inc)
    assert(rst.find((p,o) => o.flashing) == None)

  }
  test ("should return the number of flashing Octopi") {
    val inc = increment(makeOctopi(smallData))
    assert(8 == countFlashes(inc))

  }
  test("should return a list of all the currently flashing Octopi") {
    val inc = increment(makeOctopi(smallData))
    val flashing = flashers(inc).sorted
    assert(flashing.size == 8)
    val expected = List(Point(1,1), Point(2,1), Point(3,1),
                        Point(1,2),Point(3,2),
                        Point(1,3), Point(2,3), Point(3,3)
                        ).sorted
    assert(flashing == expected)
  }

  test ("update should replace the items in the first map with items in the second") {
    val octopi: Map[Point, Octopus] = Map((Point(0,0), Octopus(1)), (Point(1,1), Octopus(2)), (Point(0,1), Octopus(3)))
    val updated: Map[Point, Octopus] = Map((Point(0,0), Octopus(8)))
    val expected: Map[Point, Octopus] = Map((Point(0,0), Octopus(8)), (Point(1,1), Octopus(2)), (Point(0,1), Octopus(3)))
    val result = update(octopi, updated)
    assert(expected == result)


  }
  test("should return a list of neighbours not yet visited") {
    val p = Point(0,0)
    val b = Point(2,2)
    val neighbours = p.neighbours(b).sorted
    val notVisted = neighboursNotVisited(p, Set.empty, b).map((_,p) => p).sorted

    assert(neighbours == notVisted)

    val nMinus = neighbours.filterNot(p => p == Point(1,1)).sorted
    assert(nMinus.size == 2)
    val newNotVisited = neighboursNotVisited(p, Set((p, Point(1,1))), b).map((_,p) => p).sorted 
    assert(nMinus == newNotVisited) 
  }


  test("step1 no flashers ") {
   val testData = loadData("/day11/test.txt")
   val octopi = makeOctopi(testData)
   val result = reset(step(octopi))
   val expected = makeOctopi(loadData("/day11/test-step1.txt"))
   assert(expected == result)
  }

  test("step2 ") {
   val testData = loadData("/day11/test.txt")
   val octopi = makeOctopi(testData)
   val result = step (step(octopi))
   val expected = makeOctopi(loadData("/day11/test-step2.txt"))
   assert(expected == reset(result))
  }

  test ("should execute n steps and n resets") {
      val testData = loadData("/day11/test.txt")
      val octopi = makeOctopi(testData)
      val (result, flashCount) = step(octopi, 3, 0)
      val expected = makeOctopi(loadData("/day11/test-step3.txt"))
      assert(expected == result)

      val (result10, flashCount10) = step(octopi, 10, 0)
      val expected10 = makeOctopi(loadData("/day11/test-step10.txt"))
      assert(expected10 == result10)
      assert(204 == flashCount10)

    }
  test ("should count when all the Octopi Flash") {
     val testData = loadData("/day11/test.txt")
     val octopi = makeOctopi(testData)
     val result = targetStep(octopi)
     assert(195 == result)
  }

}

