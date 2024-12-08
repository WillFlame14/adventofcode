import scala.io.Source._

type Pair = Tuple2[Int, Int]

extension (p: Pair)
	def +(p2: Pair) =
		(p._1 + p2._1, p._2 + p2._2)

	def *(i: Int) =
		(p._1 * i, p._2 * i)

	def diff(p2: Pair) =
		(p._1 - p2._1, p._2 - p2._2)

	def inBounds(dims: (Int, Int)) =
		p._1 >= 0 && p._1 < dims._1 && p._2 >= 0 && p._2 < dims._2

	def collinear(p2: Pair) =
		val (x1, y1) = p
		val (x2, y2) = p2
		if y2 == 0 then
			y1 == 0 && (if x2 == 0 then x1 == 0 else
				(Math.max(x1.abs, x2.abs) % Math.min(x1.abs, x2.abs) == 0))
		else if x2 == 0 then
			x1 == 0
		else
			(y1.toDouble / y2) == (x1.toDouble / x2)

def findAntinodes(dims: (Int, Int), antennas: List[Pair]) =
	for
		p1 <- antennas
		p2 <- antennas if p1 != p2
		poss <- List(p2 + p2.diff(p1)) if poss.inBounds(dims)
	yield poss

def findAntinodes2(dims: (Int, Int), antennas: List[Pair]) =
	for
		x <- (0 until dims._1)
		y <- (0 until dims._2) if antennas.exists(p1 => antennas.exists(p2 =>
			p2 != p1 && p1.diff(x, y).collinear(p2.diff(x, y))))
	yield (x, y)

@main
def day8() =
	val input = fromFile("input.txt").getLines().toList

	val antennaMap: Map[Char, List[Pair]] = input.zipWithIndex.foldRight(Map())((c, a) =>
		val (line, i) = c
		line.zipWithIndex.foldRight(a)((c2, m) =>
			val (ch, j) = c2
			if ch != '.' then
				m.updated(ch, m.get(ch).map(_ :+ (j, i)).getOrElse(List((j, i))))
			else m
			)
		)

	val dims = (input(0).length, input.length)
	val List(antinodes, antinodes2) = List(findAntinodes, findAntinodes2).map(f => antennaMap.values.flatMap(as => f(dims, as)).toSet)

	val part1 = antinodes.size
	val part2 = antinodes2.size
	println((part1, part2))

	// val map = for
	// 	x <- 0 until dims._1
	// 	y <- 0 until dims._2 + 1
	// yield if y == dims._2 then '\n' else if antinodes.contains((y, x)) then '#' else '.'

	// println(map.mkString)
