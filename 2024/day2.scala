import scala.io.Source._

def safe(xs: List[Int]): Boolean =
	def loop(xs: List[Int], inc: Boolean): Boolean =
		xs match
			case Nil => true
			case x :: Nil => true
			case x :: y :: xs =>
				val same = if inc then y > x else x > y
				val good = (x - y).abs >= 1 && (x - y).abs <= 3
				same && good && loop(y :: xs, inc)
	(xs(0) - xs(1)).abs >= 1 && (xs(0) - xs(1)).abs <= 3 && loop(xs.tail, xs(1) > xs(0))

def safe2(xs: List[Int]): Boolean =
	safe(xs) || (0 until xs.length).map(i =>
		val (l, r) = xs.splitAt(i)
		l ++: r.tail
	).exists(x => safe(x))

@main
def day2() =
	val input = fromFile("input.txt").getLines()
	val in = input.map(s => s.split(" ").map(_.toInt).toList)

	val (part1, part2) = in.foldRight((0, 0))((c, a) =>
		(if safe(c) then a._1 + 1 else a._1, if safe2(c) then a._2 + 1 else a._2))

	println((part1, part2))
