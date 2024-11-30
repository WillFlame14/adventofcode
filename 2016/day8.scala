import scala.io.Source._

type Screen = Vector[Vector[Boolean]]

val WIDTH = 50
val HEIGHT= 6

object Screen:
	def empty: Screen =
		Vector.fill(HEIGHT)(Vector.fill(WIDTH)(false))

extension (s: Screen)
	def inspect: String =
		s.map(_.map(if _ then '#' else ' ').mkString).mkString("\n")

	def rect(w: Int, h: Int): Screen =
		if h <= 0 then s else
			val new_head = Vector.fill(w)(true) ++: s.head.drop(w)
			val new_tail = s.tail.rect(w, h - 1)
			new_head +: new_tail

	def rotateRow(r: Int, x: Int): Screen =
		val (left, right) = s(r).splitAt(WIDTH - x)
		s.updated(r, right ++: left)

	def rotateCol(c: Int, x: Int): Screen =
		s.transpose.rotateRow(c, WIDTH - (HEIGHT - x)).transpose

def perform(s: Screen, i: String): Screen =
	val (l, r) = i.splitAt(s.indexOf(" "))
	val rectPattern = """rect (\d+)x(\d+)""".r
	val rotateRowPattern = """rotate row y=(\d+) by (\d+)""".r
	val rotateColPattern = """rotate column x=(\d+) by (\d+)""".r

	i match
		case rectPattern(x, y) => s.rect(x.toInt, y.toInt)
		case rotateRowPattern(x, y) => s.rotateRow(x.toInt, y.toInt)
		case rotateColPattern(x, y) => s.rotateCol(x.toInt, y.toInt)

@main
def day8() =
	val input = fromFile("input.txt").getLines()

	val screen = input.foldLeft(Screen.empty)(perform(_, _))
	val part1 = screen.foldLeft(0)((a, c) => a + c.filter(x => x).length)
	val part2 = screen.inspect

	println(part1)
	println(part2)
