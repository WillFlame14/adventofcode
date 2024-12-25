package day10

import scala.io.Source._
import scala.annotation.tailrec

type Pair = Tuple2[Int, Int]

val dirs = List((0, -1), (1, 0), (0, 1), (-1, 0))

extension (p: Pair)
	def +(p2: Pair) =
		(p._1 + p2._1, p._2 + p2._2)

	def inBounds(dims: (Int, Int)) =
		p._1 >= 0 && p._1 < dims._1 && p._2 >= 0 && p._2 < dims._2

def gridAt(grid: List[List[Int]], p: Pair) =
	val (x, y) = p
	grid(y)(x)

def neighbours(dims: (Int, Int), p: Pair) =
	dirs.map(_ + p).filter(_.inBounds(dims))

def score(grid: List[List[Int]], p: Pair): Int =
	val dims = (grid(0).length, grid.length)
	def loop1(curr: Pair, i: Int): Set[Pair] =
		if i == 10 then Set(curr) else
			neighbours(dims, curr).foldRight(Set())((c, a) =>
				if gridAt(grid, c) == i then a ++ loop1(c, i + 1) else a)
	if gridAt(grid, p) == 0 then loop1(p, 1).size else 0

def rating(grid: List[List[Int]], p: Pair): Int =
	val dims = (grid(0).length, grid.length)
	def loop2(curr: Pair, i: Int): Int =
		if i == 10 then 1 else
			neighbours(dims, curr).foldRight(0)((c, a) =>
				if gridAt(grid, c) == i then a + loop2(c, i + 1) else a)
	if gridAt(grid, p) == 0 then loop2(p, 1) else 0

@main
def day10() =
	val input = fromFile("input.txt").getLines().toList

	val grid = input.map(_.split("").map(_.toInt).toList)
	val (part1, part2) = (for
		y <- 0 until grid.length
		x <- 0 until grid(y).length
	yield (score(grid, (x, y)), rating(grid, (x, y)))).foldRight((0, 0))(_ + _)

	println((part1, part2))
