import scala.io.Source._
import aoc._

type Grid = List[List[Char]]

def dirMap = Map(('^', (0, -1)), ('>', (1, 0)), ('v', (0, 1)), ('<', (-1, 0)))

def parse(s: List[String]) =
	val grid = s.map(_.toCharArray.toList)

	val start = (for
		y <- 0 until grid.length
		x <- 0 until grid(y).length
	yield (x, y)).find(p => gridAt(grid, p) == '@').get

	val newGrid = grid.map(line =>
		val i = line.indexOf('@')
		if i == -1 then line else line.updated(i, '.'))
	(newGrid, start)

def expand(grid: Grid) =
	grid.map(_.flatMap(_ match
		case '.' => List('.', '.')
		case 'O' => List('[', ']')
		case '#' => List('#', '#')
	))

def update(grid: Grid, p: Pair[Int], move: Char) =
	val dir = dirMap(move)
	val horiz = move == '<' || move == '>'

	def movable(curr: Pair[Int]): Boolean =
		gridAt(grid, curr) match
			case '#' => false
			case '.' => true
			case 'O' => movable(curr + dir)
			case '[' => movable(curr + dir) &&
				(if horiz then true else movable(curr + dir + (1, 0)))
			case ']' => movable(curr + dir) &&
				(if horiz then true else movable(curr + dir + (-1, 0)))

	if movable(p + dir) then
		def updateGrid(curr: Pair[Int], newGrid: Grid, item: Char): Grid =
			val next = curr + dir
			val nextGrid = updateAt(newGrid, next, item)
			gridAt(newGrid, next) match
				case '.' => nextGrid
				case 'O' => updateGrid(next, nextGrid, 'O')
				case '[' =>
					val lGrid = updateGrid(next, nextGrid, '[')
					if horiz then lGrid else updateGrid(next + (1, 0), updateAt(lGrid, next + (1, 0), '.'), ']')
				case ']' =>
					val rGrid = updateGrid(next, nextGrid, ']')
					if horiz then rGrid else updateGrid(next + (-1, 0), updateAt(rGrid, next + (-1, 0), '.'), '[')

		val currChar = gridAt(grid, p + dir)
		if "[]O".contains(currChar.toString) then
			(updateGrid(p, grid, '.'), p + dir)
		else
			(grid, p + dir)
	else (grid, p)

def displayGrid(grid: Grid, p: Pair[Int]) =
	println(updateAt(grid, p, '@').map(_.mkString + "\n").mkString)

def gpsScore(grid: Grid) =
	(for
		y <- 0 until grid.length
		x <- 0 until grid(y).length if gridAt(grid, (x, y)) == 'O' || gridAt(grid, (x, y)) == '['
	yield (x, y)).map(p => p._2 * 100 + p._1).sum

@main
def day15() =
	val input = fromFile("input.txt").getLines().toList

	val (gridmap, insts) = input.splitAt(input.indexOf(""))
	val dirs = insts.tail.mkString.toCharArray.toList

	val (grid, start) = parse(gridmap)
	val (fGrid, fPos) = dirs.foldLeft((grid, start))((a, c) => update(a._1, a._2, c))
	val part1 = gpsScore(fGrid)

	val expandStart = (start._1 * 2, start._2)
	val (fGrid2, fPos2) = dirs.foldLeft((expand(grid), expandStart))((a, c) => update(a._1, a._2, c))
	val part2 = gpsScore(fGrid2)

	println((part1, part2))
