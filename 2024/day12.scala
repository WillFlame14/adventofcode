import scala.io.Source._

type Pair = Tuple2[Int, Int]

val dirs = List((0, -1), (1, 0), (0, 1), (-1, 0))

extension (p: Pair)
	def +(p2: Pair) =
		(p._1 + p2._1, p._2 + p2._2)

	def -(p2: Pair) =
		(p._1 - p2._1, p._2 - p2._2)

	def abs =
		Math.abs(p._1) + Math.abs(p._2)

	def inBounds(dims: (Int, Int)) =
		p._1 >= 0 && p._1 < dims._1 && p._2 >= 0 && p._2 < dims._2

def gridAt[A](grid: List[List[A]], p: Pair) =
	val (x, y) = p
	grid(y)(x)

def neighbours(dims: (Int, Int), p: Pair) =
	dirs.map(_ + p).filter(_.inBounds(dims))

def area(grid: List[List[Char]], start: Pair): Set[Pair] =
	val dims = (grid(0).length, grid.length)
	val plant = gridAt(grid, start)

	def loop(curr: Pair, visited: Set[Pair]): Set[Pair] =
		val valid = neighbours(dims, curr).filter(n =>
			n.inBounds(dims) && gridAt(grid, n) == plant && !visited.contains(n))
		valid.foldRight(visited ++ valid)((c, a) => loop(c, a))

	loop(start, Set(start))

def allAreas(grid: List[List[Char]]): List[(Char, Set[Pair])] =
	val coords = for
		y <- 0 until grid.length
		x <- 0 until grid(y).length
	yield (x, y)

	val (areas, _) = coords.foldRight((Nil: List[(Char, Set[Pair])], Set(): Set[Pair]))((c, a) =>
		val (res, visited) = a
		if visited.contains(c) then a else
			val group = area(grid, c)
			(res :+ (gridAt(grid, c), group), visited ++ group)
	)
	areas

/** Counts the number of outer neighbours that aren't part of the group. */
def perimeter(group: Set[Pair]) =
	group.toList.map(p => dirs.map(_ + p).filter(n => !group.contains(n)).length).sum

def perimeter2(group: Set[Pair]) =
	// List of (dir, outerNeighbour) that each represent a fence segment
	val outers = group.toList.flatMap(p =>
		dirs.map(d => (d, d + p)).filter(n => !group.contains(n._2)))

	// If an adjacent fence segment exists that was generated with the same dir,
	// then it can be combined (i.e. don't count this segment)
	val filtered = outers.filter(o =>
		val (dir, p) = o
		!outers.exists(o1 =>
			val (dir1, p1) = o1
			dir1 == dir && (p1 - p).abs == 1 &&
			(p1._1 < p._1 || p1._2 < p._2)))
	filtered.length

@main
def day12() =
	val input = fromFile("input.txt").getLines().map(_.toList).toList

	val areas = allAreas(input)
	val perimeters = List(perimeter, perimeter2).map(f => areas.map(a => f(a._2)))

	val List(part1, part2) = perimeters.map(p => areas.zip(p).map(x =>
		val (a, p) = x
		a._2.size * p).sum)

	println((part1, part2))
