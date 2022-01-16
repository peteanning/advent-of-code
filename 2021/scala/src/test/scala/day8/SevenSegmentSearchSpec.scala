package day8

import SevenSegmentSearch.*


class SevenSegmentSearchSpec extends org.scalatest.funsuite.AnyFunSuite {
  
  def testfile: String = "/day8/test.txt"
  
  def  contents: List[String] = loadData(testfile)
  
  def parsedData = splitSignalsAndObservations(contents)

  def s = splitSignalsAndObservations(List("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

  def knowns = convertSignalsToKnownDigits(s(0)._1)

  def all = findOverlaps(knowns)


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
    assert(all.size == 10) 
    assert(all.exists((k,v) => v == None) == false)
    assert(all.get("acedgfb").get == Some(8))
    assert(all.get("cdfbe").get == Some(5))
    assert(all.get("gcdfa").get == Some(2))
    assert(all.get("fbcad").get == Some(3))
    assert(all.get("dab").get == Some(7))
    assert(all.get("cefabd").get == Some(9))
    assert(all.get("cdfgeb").get == Some(6))
    assert(all.get("eafb").get == Some(4))
    assert(all.get("cagedb").get == Some(0))
    assert(all.get("ab").get == Some(1))
  }
  test("should decode the digits") {
    val decoded = decodeDigits(s(0)._2, all)
    assert(decoded == 5353)
  }
  test("shoud process the whole file and sum the digits") {
    assert(processData(testfile) == 61229)
  }
  test("should convert a list of Ints to an Int List(1,2,3,4) = 1234 ") {
    assert(listIntToInt(List(1,2,3,4)) == 1234)
    assert(listIntToInt(List(1,0,0,4)) == 1004)
    assert(listIntToInt(List(0,0,0,0)) == 0)

  }
}
