import scala.io.Source._

val decompPattern = """([A-Z0-9]*?)\((\d+)x(\d+)\)(.*)""".r

def decompress(s: String): String =
	s match
		case decompPattern(pre, a, b, post) =>
			val (rep, rest) = post.splitAt(a.toInt)
			pre ++: rep * b.toInt ++: decompress(rest)
		case _ => s

def decompressLen(s: String): BigInt =
	s match
		case decompPattern(pre, a, b, post) =>
			val (rep, rest) = post.splitAt(a.toInt)
			pre.length + decompressLen(rep) * b.toInt + decompressLen(rest)
		case _ => s.length

@main
def day9() =
	val input = fromFile("input.txt").getLines().next

	val part1 = decompress(input).length
	val part2 = decompressLen(input)

	println((part1, part2))
