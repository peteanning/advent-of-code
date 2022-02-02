package day10

import SyntaxScorer._

class SyntaxScorerSpec extends org.scalatest.funsuite.AnyFunSuite {

  def data: List[List[String]] = loadData("/day10/test.txt")
  def example1 = "[({(<(())[]>[[{[]{<()<>>".toArray.map(_.toString).toList
  def example2 = "[(()[<>])]({[<{<<[]>>(".toArray.map(_.toString).toList
  def example3 = "(((({<>}<{<{<>}{[]{[]{}".toArray.map(_.toString).toList
  def example4 = "{<[[]]>}<{[{[{[]{()[[[]".toArray.map(_.toString).toList
  def example5 = "<{([{{}}[<[[[<>{}]]]>[]]".toArray.map(_.toString).toList

  test("Load Data test") {
    assert(data.length == 10)
    assert(data(0)(0) == "[")
    assert(data(9)(23) == "]")
  }
  
  test("Should check an open state") {
   assert(isOpen(List(Parens.OPEN_ANGLE)) == true)
   assert(isOpen(List(Parens.OPEN_SQUARE)) == true)
   assert(isOpen(List(Parens.OPEN_CURVE)) == true)
   assert(isOpen(List(Parens.OPEN_CURLY)) == true)
   assert(isOpen(List.empty) == true)
  }

  test("Should check a symbol is an opening type") {
    assert(isOpen("(") == true)
    assert(isOpen("[") == true)
    assert(isOpen("<") == true)
    assert(isOpen("{") == true)
  }

  test("should pare a List of symbols") {
    assert(parseCorupted(List.empty[String]) == None)
    assert(parseCorupted(List("(", ")")) == None)
    assert(parseCorupted(List("(", "<", "{", "[", "]", "}", ">", ")")) == None)
  }
  test("should find and score errors") {
    assert(parseCorupted(List("<", ")")) == Some(3)) 
    assert(parseCorupted(List("<", "(", "{", "}", "[", ")")) == Some(3)) 
    assert(parseCorupted(List("<", "}")) == Some(1197)) 
    assert(parseCorupted(List("<", "]")) == Some(57)) 
    assert(parseCorupted(List("(", ">")) == Some(25137)) 
  }

  test("should parse file of strings of symbols and calculate the total score") {
    assert(parseCorupted(data) == 26397)
  } 
  test("should parse and find incomplete lines") {
     assert(parseIncomplete(List("<", "{", "(", "[")) == Some(294) )
  }

  test("should parse incomplete data") {
    assert(parseIncomplete(data) == 288957)
  }
  test("should parse incomplete data lines") {
    assert(parseIncomplete(example1) == Some(288957))
    assert(parseIncomplete(example2) == Some(5566))
    assert(parseIncomplete(example3) == Some(1480781))
    assert(parseIncomplete(example4) == Some(995444))
    assert(parseIncomplete(example5) == Some(294))
    val bugHunt = "{, [, <, <, (, [, {, (, (, (, (, <, [, <, [, ], {, }, >, <, [, ], {, }, >, ], <, <, {, }, {, }, >, <, [, ], <, >, >, >, >, <, <, (, [, ], <, >, ), >, [, (, {, }, [, ], ), (, {, }, [, ], ), ], >, ), ), (, [, [, (, {, [, ], {, }, }, ), <, {, {, }, {, }, }, (, (, ), ), >, ], [, [, {"
    val bugHuntList = bugHunt.split(',').map(_.trim).toList
    
    assert(parseIncomplete(bugHuntList).get > 0 )
  }
  test("check input data load") {
    val input = loadData("/day10/input.txt")
    assert(input.length == 94)
    val lastLine = "<[(({<[({<[{{{()()}<<>[]}}({<>[]}<[]<>>)}]{[<<[]()><()()>>[({}<>){[]{}}]][<<()()>[<>]>{{()<>}}]}>})]>}))({".toArray
    val lastList = lastLine.map(_.toString).toList
    val firstLine = "{[<<([{((((<[<[]{}><[]{}>]<<{}{}><[]<>>>><<([]<>)>[({}[])({}[])]>))([[({[]{}})<{{}{}}(())>][[{".toArray
    val firstList = firstLine.map(_.toString).toList

    assert(input(0) == firstList)
    assert(input(93) == lastList)
  }
}
