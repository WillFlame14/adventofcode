import scala.io.Source._

type Pair = Tuple2[Int, Int]

extension (p: Pair)
	def +(p2: Pair) =
		(p._1 + p2._1, p._2 + p2._2)

val dirs = List((0, -1), (1, 0), (0, 1), (-1, 0))

def rotate_cw(dir: Pair) =
	val i = dirs.indexOf(dir)
	dirs((i + 1) % dirs.length)

def travel(grid: Map[Int, Set[Int]], dims: Pair, curr: Pair, dir: Pair, visited: Set[Pair]): Set[Pair] =
	val next = curr + dir
	val (x, y) = next
	if x < 0 || x >= dims._1 || y < 0 || y >= dims._2 then visited else
		if grid.get(x).map(_.contains(y)).getOrElse(false) then
			travel(grid, dims, curr, rotate_cw(dir), visited + curr)
		else travel(grid, dims, next, dir, visited + curr)

def parse_grid(s: List[String]): Map[Int, Set[Int]] =
	(for
		y <- 0 until s.length
		x <- 0 until s(y).length if s(y).charAt(x) == '#'
	yield (x, y)).foldRight(Map())((c, a) =>
		val (x, y) = c
		a.updated(x, a.get(x).map(_ + y).getOrElse(Set(y))))

def parse_start(s: List[String]) =
	(for
		y <- 0 until s.length
		x <- 0 until s(y).length if s(y).charAt(x) == '^'
	yield (x, y))(0)

def paradox(grid: Map[Int, Set[Int]], dims: Pair, curr: Pair, dir: Pair, visited: Set[(Pair, Pair)]): Boolean =
	val next = curr + dir
	val (x, y) = next
	val hash = (curr, dir)

	// If we've visited this spot before while facing the same direction, we know we're in a loop.
	if visited.contains(hash) then true else
		if x < 0 || x >= dims._1 || y < 0 || y >= dims._2 then false else
			if grid.get(x).map(_.contains(y)).getOrElse(false) then
				paradox(grid, dims, curr, rotate_cw(dir), visited + hash)
			else paradox(grid, dims, next, dir, visited + hash)

@main
def day6() =
	val input = fromFile("input.txt").getLines().toList
	val grid = parse_grid(input)
	val start = parse_start(input)
	val dims = (input(0).length, input.length)
	val visited = travel(grid, dims, start, (0, -1), Set(start))

	val part1 = visited.size

	// Try placing a wall on each visited loc except start
	val part2 = visited.filter(p =>
		val (x, y) = p
		p != start && paradox(grid.updated(x, grid.get(x).map(_ + y).getOrElse(Set(y))), dims, start, (0, -1), Set())).size

	println((part1, part2))
