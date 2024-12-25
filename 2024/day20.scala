package day20

import scala.io.Source._
import aoc._
import scala.annotation.tailrec

type PPair = (Pair[Int], List[Pair[Int]], Set[Pair[Int]])
type Grid = List[List[Char]]

def parse(s: List[String]) =
	val grid = s.map(_.toCharArray.toList)

	val start = (for
		y <- 0 until grid.length
		x <- 0 until grid(y).length
	yield (x, y)).find(p => gridAt(grid, p) == 'S').get

	val finish = (for
		y <- 0 until grid.length
		x <- 0 until grid(y).length
	yield (x, y)).find(p => gridAt(grid, p) == 'E').get

	(updateAt(updateAt(grid, start, '.'), finish, '.'), start, finish)

def cheat(grid: Grid, shortestPath: List[Pair[Int]]) =
	val pathMap = shortestPath.zipWithIndex.toMap
	val dims = (grid(0).length, grid.length)

	shortestPath.foldLeft(Map(): Map[(Pair[Int], Pair[Int]), Int])((a, c) =>
		val wallNeighbours = neighbours(dims, c).filter(p => (p - (1, 1)).inBounds(dims - (2, 2)) && gridAt(grid, p) == '#').toSet
		val finalNeighbours = wallNeighbours.flatMap(wc =>
			neighbours(dims, wc).filter(p => gridAt(grid, p) == '.' && pathMap(p) - pathMap(c) - 2 >= 100).map(p => (wc, p)))

		finalNeighbours.foldRight(a)((n, ma) => ma.updated(n, pathMap(n._2) - pathMap(c) - 2)))

def cheat2(grid: Grid, shortestPath: List[Pair[Int]]) =
	val pathMap = shortestPath.zipWithIndex.toMap
	val dims = (grid(0).length, grid.length)

	@tailrec def findCheats(start: Pair[Int], frontier: List[(Pair[Int], Int)], visited: Set[Pair[Int]], cheats: Int = 0): Int =
		if frontier.isEmpty then cheats else
			val (curr, i) = frontier.head
			val validNeighbours = neighbours(dims, curr).filter(p => !visited.contains(p))

			val newFrontier = if i < 19 then frontier.tail :++ validNeighbours.map(p => (p, i + 1)) else frontier.tail
			val newCheats = validNeighbours.filter(p => gridAt(grid, p) == '.' && pathMap(p) - pathMap(start) - i - 1 >= 100).length

			findCheats(start, newFrontier, visited ++ validNeighbours, cheats + newCheats)

	shortestPath.foldLeft(0)((a, c) => a + findCheats(c, List((c, 0)), Set(c)))

@main
def day20() =
	val input = fromFile("input.txt").getLines().toList

	val (grid, start, finish) = parse(input)

	val obstacles = (for
		y <- 0 until grid.length
		x <- 0 until grid(y).length if gridAt(grid, (x, y)) == '#'
	yield (x, y)).toSet

	val shortest = shortestPath((grid(0).length, grid.length), start, finish, obstacles).get

	val cheats = cheat(grid, shortest)
	val part1 = cheats.toList.groupBy((a, b) => b).toList.sortBy(_._1).map((a, b) => (a, b.length)).foldRight(0)(_._2 + _)
	println(part1)

	val part2 = cheat2(grid, shortest)
	println(part2)
