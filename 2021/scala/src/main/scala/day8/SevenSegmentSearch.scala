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
                     acc + 1
                   else 
                     acc
      }
    }
    parsedData.foldLeft(0)((acc, x) =>  acc + _countUniqueDigits(x._2))

  def convertSignalsToKnownDigits(signals: List[String]): Map[String, Option[Int]] =
      signals.foldLeft(Map.empty[String, Option[Int]]){
        (acc, value) => 
              val maybeNumber = lookup(value)
              maybeNumber match {
                case n@Some(x) => acc ++ Map(value -> Some(x))
                case _ => acc ++ Map(value -> None)
        }
      }

    // the digit 3 has 5 segments and overlaps the digit 7 in 3 of those
    // the digit 2 has 5 segments and overlaps the digit 4 in 2 of those
    // the digit 5 has 5 segments and overlaps the digit 4 in 3 of those
    // the digit 9 has 6 segments and overlaps the digit 3 in 5 of those
    // the digit 0 has 6 segments and overlaps the digit 1 in 2 of those
    // the digit 6 is left over
  def findOverlaps(curentMappings: Map[String, Option[Int]]): Map[String, Option[Int]] =
    
    val knowns = curentMappings.filterNot ((k,v) => v == None)
    val unknowns: Map[String, Option[Int]] = curentMappings.filter ((k,v) => v == None) 

    def getKnown (n: Int, k: Map[String, Option[Int]]) = 
      k.find(_._2 == Some(n)).map(_._1).getOrElse("")
    
    def findUnknown (n: Int, overlap: Overlap, toSearchIn: Map[String, Option[Int]]): Map[String, Option[Int]] =
      val found = toSearchIn.find {
                    (k,v) => 
                      (k intersect overlap.withStr).length == overlap.by 
                      && k.length == overlap.segments
                      && v == None // for safety
      }
      toSearchIn + (found.get._1 -> Some(n)) //this should boot out the one we found and replace it

    val three = findUnknown(3, Overlap(getKnown(7, knowns), 3, 5), unknowns)
    val two = findUnknown(2, Overlap(getKnown(4, knowns), 2, 5), three)
    val five = findUnknown(5, Overlap(getKnown(4, knowns), 3, 5), two)
    val nine = findUnknown(9, Overlap(getKnown(3, five), 5, 6), five) 
    val zero = findUnknown(0, Overlap(getKnown(1, knowns), 2, 6), nine)
    assert(zero.count(_._2 == None) == 1, "check there is only one candiate for a 6")
    knowns ++ zero.map((k,v) => if v == None then (k, Some(6)) else (k,v))
    
  def decodeDigits(displays: List[String], mappings: Map[String, Option[Int]]): Int =
    val sortedDisplays = displays.map(_.sorted)
    val sortedMappings = mappings.map((k,v) => (k.sorted, v))
    val decodedDigits = sortedDisplays.map(s => sortedMappings.get(s).flatten.get)
    decodedDigits.foldRight((0,1))( (c, acc) => (acc._1 + (c * acc._2), acc._2 * 10))._1

  def processData(classPathName: String): Int =
   val data = loadData(classPathName)
   val split: List[(List[String], List[String])] = splitSignalsAndObservations(data)
     split.map {
     (signals, displays) => 
       val decodedSignals = findOverlaps(convertSignalsToKnownDigits(signals))
       val decodedDisplay = decodeDigits(displays, decodedSignals)
       decodedDisplay
   }.sum


case class Overlap(withStr: String, by: Int, segments: Int)
