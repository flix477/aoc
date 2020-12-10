import scala.io.Source

object Main {
  type Seat = (Int, Int)

  def main(args: Array[String]) = {
    val seatIds = Source.fromFile("input.txt").getLines().
      map(parseSeatId).toList.sorted

    println(seatIds.last)
    println(findSeat(seatIds))
  }

  def translate(f: Char => Int)(input: String) = input
    .map(f)
    .foldLeft(0)(_ << 1 | _)

  val fbToBit: (Char) => Int = {
    case 'F' => 0
    case 'B' => 1
  }

  val rlToBit: (Char) => Int = {
    case 'R' => 1
    case 'L' => 0
  }

  val row: (String => Int) = translate(fbToBit)
  val col: (String => Int) = translate(rlToBit)

  def parse(input: String) = {
    val (rowStr, colStr) = input.splitAt(7)
    (row(rowStr), col(colStr))
  }

  val seatId: (Seat => Int) = { case (row, col) => row * 8 + col }

  // poor man's function composition
  val parseSeatId: (String) => Int = x => seatId(parse(x))

  val findSeat: (List[Int] => Option[Int]) = {
    case a :: b :: rest => if (a + 1 == b - 1) Some(a + 1) else findSeat(b::rest)
    case _ => None
  }
}
