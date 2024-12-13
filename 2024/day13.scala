package aoc
import scala.io.Source._

def press(target: Point, aButton: Point, bButton: Point): Option[Long] =
	val (a, d) = aButton
	val (b, e) = bButton
	val (c, f) = target

	val det = a*e - b*d

	val (x, y) = ((c*e - b*f) / det, (a*f - c*d) / det)
	if ((c*e - b*f) % det == 0) && ((a*f - c*d) % det == 0) then
		Some(x.toLong*3 + y.toLong) else None

val buttonPattern = """Button .: X\+(\d+), Y\+(\d+)""".r
val prizePattern = """Prize: X=(\d+), Y=(\d+)""".r

def parsePoint(s: String): Point =
	s match
		case buttonPattern(x, y) => (x.toInt, y.toInt)
		case prizePattern(x, y) => (x.toInt, y.toInt)

def parse(lines: Seq[String], part2: Boolean) =
	val List(aButton, bButton, target) = lines.take(3).toList.map(parsePoint)
	press(if part2 then target + (10000000000000L, 10000000000000L) else target, aButton, bButton)

@main
def day13() =
	val input = fromFile("input.txt").getLines().grouped(4).toList

	val res = List(false, true).map(b => input.map(ls => parse(ls, b))
		.foldRight(0L)((c, a) => c.getOrElse(0L) + a))
	println(res)
