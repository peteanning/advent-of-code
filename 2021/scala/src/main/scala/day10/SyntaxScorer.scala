package day10

object SyntaxScorer extends App:

  def loadData(fileName: String): List[List[String]] =
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(fileName))
    val text = try source.getLines.toList finally source.close()
    text.map(_.toArray.toList.map(_.toString))

  def isOpen(state: List[Parens]): Boolean =
      state.isEmpty || state.head == Parens.OPEN_SQUARE || state.head == Parens.OPEN_CURVE 
        || state.head == Parens.OPEN_CURLY || state.head == Parens.OPEN_ANGLE

  def isOpen(symbol: String): Boolean = 
    symbol == "[" || symbol == "(" || symbol == "{" || symbol == "<"

  val errorScore: Map[String, Int] = Map(")" -> 3, "]" -> 57, "}" -> 1197, ">" -> 25137)
  val closingScore: Map[Parens, Int] = Map(Parens.OPEN_ANGLE -> 4, Parens.OPEN_CURLY -> 3, 
                                           Parens.OPEN_SQUARE -> 2, Parens.OPEN_CURVE -> 1)
  
 
  def parseIncomplete(lines: List[List[String]]): Long =
    val parsed: List[Option[Long]] = lines.map(l => parseIncomplete(l))
    val parsedFiltered = parsed.filterNot(_ == None)
    val midpointIndex = (parsedFiltered.length - 1) / 2
    val sorted = parsedFiltered.sorted
    //println(parsed)
    sorted(midpointIndex).getOrElse(0l)
    
  def parseIncomplete(completeLine: List[String]): Option[Long] =
    def _parse(state: List[Parens], line: List[String]): Option[Long] =
      line match {
        case l :: ls if isOpen(state) && isOpen(l) => _parse(Parens(l) :: state, ls)
        case l :: ls if isOpen(state) && !isOpen(l) =>  {
          val closingSymbol = Parens(l)
          val matchingState = Parens.invert(state.head)
          if matchingState == closingSymbol then
            _parse(state.tail, ls)
          else
           None
        }
        case Nil if state.nonEmpty => {
          Some(state.foldLeft(0l)(((acc, current) => (5 * acc) + closingScore.get(current).get)))
        }
        case Nil if state.isEmpty => None
        case _  => throw new Error("Unknown state")

         
      }
    _parse(List.empty, completeLine)

  def parseCorupted(lines: List[List[String]]): Int =
    val parsed: List[Option[Int]] = lines.map(l => parseCorupted(l))
    parsed.foldLeft(Some(0))((e, acc) => Some(e.getOrElse(0) + acc.getOrElse(0))).getOrElse(0)
 
  def parseCorupted(completeLine: List[String]): Option[Int] =
    def _parse(state: List[Parens], line: List[String]): Option[Int] =
      line match {
        case l :: ls if isOpen(state) && isOpen(l) => _parse(Parens(l) :: state, ls)
        case l :: ls if isOpen(state) && !isOpen(l) =>  {
          val closingSymbol = Parens(l)
          val matchingState = Parens.invert(state.head)
          if matchingState == closingSymbol then
            _parse(state.tail, ls)
          else
            Some(errorScore.getOrElse(l, 0))
        }
        case Nil => None
        case _ => throw new Error("unknown state")
         
      }
    _parse(List.empty, completeLine)

  enum Parens(symbol: String):
     case OPEN_SQUARE extends Parens("[")   
     case OPEN_CURLY extends Parens("{")   
     case OPEN_CURVE extends Parens("(")   
     case OPEN_ANGLE extends Parens("<")   
     case CLOSE_SQUARE extends Parens("]")   
     case CLOSE_CURLY extends Parens("}")   
     case CLOSE_CURVE extends Parens(")")   
     case CLOSE_ANGLE extends Parens(">")   

  object Parens:
    def apply(symbol: String) = symbol match {
      case "[" => OPEN_SQUARE
      case "{" => OPEN_CURLY
      case "(" => OPEN_CURVE
      case "<" => OPEN_ANGLE
      case "]" => CLOSE_SQUARE
      case "}" => CLOSE_CURLY
      case ")" => CLOSE_CURVE
      case ">" => CLOSE_ANGLE
      case _ => throw new Error(s"Unknown symbol $symbol")
      }
    
    def invert(symbol: Parens) = symbol match {
      case OPEN_ANGLE => CLOSE_ANGLE
      case OPEN_CURLY => CLOSE_CURLY
      case OPEN_CURVE => CLOSE_CURVE
      case OPEN_SQUARE => CLOSE_SQUARE
      case _ => throw new Error("Only inverts opening Parens") 
    }


  


