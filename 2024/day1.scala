import scala.io.Source._

@main
def day8() =
	val input = fromFile("input.txt").getLines()

	val pattern = """(\d+) *(\d+)""".r

	val (l1, l2) = input.foldRight((Nil: List[Int], Nil: List[Int]))((c, a) =>
		c match
			case pattern(x, y) => (x.toInt +: a._1, y.toInt +: a._2))

	val part1 = l1.sorted.zip(l2.sorted).foldLeft(0)((a, c) => a + (c._1 - c._2).abs)
	val part2 = l1.foldLeft(0)((a, c) => a + c * l2.filter(_ == c).length)

	println((part1, part2))
