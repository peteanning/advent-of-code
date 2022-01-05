package day8

object SevenSegmentSearch extends App:
  // day8.SevenSegmentSearch.loadData("/day8/test.txt")
  val D1_LEN = 2
  val D4_LEN = 4
  val D7_LEN = 3
  val D8_LEN = 7

  def loadData(fileName: String): List[String] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text

  def splitSignalsAndObservations(rawData: List[String]): List[(String, List[String])] =
    rawData.map {
        s => 
            val splitLine = s.split('|')
            (splitLine(0).trim, splitLine(1).trim.split(' ').toList)
      }

  def countUniqueDigits(parsedData: List[(String, List[String])]) =
    val segmentsLengths = Set(D1_LEN, D4_LEN, D7_LEN, D8_LEN)
    val trueMap = Map((D1_LEN, 1), (D4_LEN, 4), (D7_LEN, 7), (D8_LEN, 8))


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

