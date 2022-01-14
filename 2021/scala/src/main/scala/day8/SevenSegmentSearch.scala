package day8

object SevenSegmentSearch extends App:
  // day8.SevenSegmentSearch.loadData("/day8/test.txt")
  val D1_LEN = 2
  val D4_LEN = 4
  val D7_LEN = 3
  val D8_LEN = 7

  val segmentsLengths = Set(D1_LEN, D4_LEN, D7_LEN, D8_LEN)
  val trueMap = Map((D1_LEN, 1), (D4_LEN, 4), (D7_LEN, 7), (D8_LEN, 8))
  def lookup (s: String) = trueMap.get(s.length)

  def loadData(fileName: String): List[String] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text

  def spaceDelimitedToList(delimited: String):List[String] =
    delimited.split(' ').toList

  def splitSignalsAndObservations(rawData: List[String]): List[(List[String], List[String])] =
    
    rawData.map {
        s => 
            val splitLine = s.split('|')
            val digits = spaceDelimitedToList(splitLine(1).trim)
            val signals = spaceDelimitedToList(splitLine(0).trim)
            (signals, digits)
      }

  def countUniqueDigits(parsedData: List[(List[String], List[String])]) =
    def _countUniqueDigits(observed: List[String]) = {
      observed.foldLeft(0){
        (acc,s) => if segmentsLengths.contains(s.length) then
                      //println(s"$s = ${trueMap.getOrElse(s.length, -99)}")
                      acc + 1
                   else 
                     acc
      }
    }
    parsedData.foldLeft(0)((acc, x) =>  acc + _countUniqueDigits(x._2))

  def convertSignalsToKnownDigits(signals: List[String]): Map[String, Option[Int]] =
      val s = signals.foldLeft(Map.empty[String, Option[Int]]){
        (acc, value) => 
              val maybeNumber = lookup(value)
              maybeNumber match {
                case n@Some(x) => acc ++ Map(value -> Some(x))
                case _ => acc ++ Map(value -> None)
        }
      }
      println(s)
      s

    // the digit 3 has 5 segments and overlaps the digit 7 in 3 of those
    // the digit 2 has 5 segments and overlaps the digit 4 in 2 of those
    // the digit 5 has 5 segments and overlaps the digit 4 in 3 of those
    // the digit 9 has 6 segments and overlaps the digit 3 in 5 of those
    // the digit 0 has 6 segments and overlaps the digit 1 in 2 of those
    // the digit 6 is left over
  def findOverlaps(knowns: Map[String, Option[Int]]): Map[String, Int] =
    def getKnown(n: Int) =
      knowns.find(_._2 == Some(n)).map(_._1).get
 
        
    val seven: String = getKnown(7)
    val one: String = getKnown(1)
    val four: String = getKnown(4)
    val eight: String = getKnown(8)

    val unknowns = knowns.filter(_._2 == None)
    
    def mapKnown (values: Map[String,Option[Int]], overlapWith: String, by: Int): String =
       val r = values.find{
           (k, v) => (k intersect overlapWith).length == by
       }
       r.get._1


    val three: String = mapKnown(unknowns, seven, 3)
    val two: String  =  mapKnown(unknowns.filterNot(_._1 == three), four, 2)
    val five: String = mapKnown(unknowns, four, 3)
    val nine: String = mapKnown(unknowns, three, 5)
    val zero: String = mapKnown(unknowns, one, 2)
    val six: String = knowns.filter{ 
             (k, v) => List(one, two, three, four, five, seven, eight, nine).contains(k)
        }.head._1

    Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)






