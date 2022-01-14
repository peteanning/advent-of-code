package day8

import SevenSegmentSearch.*


class SevenSegmentSearchSpec extends org.scalatest.funsuite.AnyFunSuite {
  
  def testfile: String = "/day8/test.txt"
  
  def  contents: List[String] = loadData(testfile)
  
  def parsedData = splitSignalsAndObservations(contents)

  def s = splitSignalsAndObservations(List("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

  def knowns = convertSignalsToKnownDigits(s(0)._1)


  test("SevenSegmentSearch loadData") {
    assert(contents.length == 10)
  }

  test("SevenSegmentSearch splitSignalsAndObservations") {
    assert(parsedData.length == 10)
  }

  test("should pare the whole line correctly") {

    assert(s(0)._1.length == 10)
    assert(s(0)._1(0) == "acedgfb")
    assert(s(0)._1(9) == "ab")

    assert(s(0)._2.length == 4)
    assert(s(0)._2(0) == "cdfeb")
    assert(s(0)._2(3) == "cdbaf")
  }

  test("counting the unique digits in the obersved data") {
    assert(countUniqueDigits(parsedData) == 26)   
  }

  test("should split a space delimited string into a list") {
    val list = spaceDelimitedToList("this should split into a list")
    assert(list.length == 6)   
    assert(list(0) == "this")
    assert(list(1) == "should")
    assert(list(2) == "split")
    assert(list(3) == "into")
    assert(list(4) == "a")
    assert(list(5) == "list")
  }

  test("should convert strings representing 1, 4, 7, 8") {

    assert(knowns.get("ab").get == Some(1))
    assert(knowns.get("dab").get == Some(7))
    assert(knowns.get("eafb").get == Some(4))
    assert(knowns.get("acedgfb").get == Some(8))

  }

  test("should find the overlaps between known and unknowns and map them to a full set of Digits") {
    val all = findOverlaps(knowns)
    assert(all.size == 10) 

  }

}
