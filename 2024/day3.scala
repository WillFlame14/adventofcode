import scala.io.Source._

val mulPattern = """mul\((\d+),(\d+)\)""".r
val ablePattern = """(.*?)(do\(\)|don't\(\))(.*)""".r

def mulResult(s: String): Int =
	val matches = mulPattern.findAllIn(s).matchData
	matches.map(_.subgroups.map(_.toInt).product).sum

def ableMul(s: String, on: Boolean): Int =
	s match
		case ablePattern(curr, op, rest) =>
			(if on then mulResult(curr) else 0) + ableMul(rest, op == "do()")
		case _ => mulResult(s)

@main
def day3() =
	val input = fromFile("input.txt").getLines().mkString

	val part1 = mulResult(input)
	val part2 = ableMul(input, true)

	println((part1, part2))
