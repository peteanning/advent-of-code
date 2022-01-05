package day8

import SevenSegmentSearch.*


class SevenSegmentSearchSpec extends org.scalatest.funsuite.AnyFunSuite {
  
  def testfile: String = "/day8/test.txt"
  
  def  contents: List[String] = loadData(testfile)
  
  def parsedData = splitSignalsAndObservations(contents)

  test("SevenSegmentSearch loadData") {
    assert(contents.length == 10)
  }
  test("SevenSegmentSearch splitSignalsAndObservations") {
    assert(parsedData.length == 10)
  }
  test("should pare the whole line correctly") {
    val s = splitSignalsAndObservations(List("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
    assert(s(0)._2.length == 4)
    assert(s(0)._2(0) == "cdfeb")
    assert(s(0)._2(1) == "fcadb")
    assert(s(0)._2(2) == "cdfeb")
    assert(s(0)._2(3) == "cdbaf")
  }
  test("counting the unique digits in the obersved data") {
    assert(countUniqueDigits(parsedData) == 26)   
  }
}
