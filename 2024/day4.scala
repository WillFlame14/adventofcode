package day4

import scala.io.Source._
import scala.annotation.tailrec

val dirs = List((0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

@tailrec def _xmas(grid: Vector[Vector[Char]], curr: (Int, Int), dir: (Int, Int), acc: Int, goal: String): Boolean =
	if acc == goal.length then true else
		val (x, y) = curr
		val outOfBounds = x < 0 || x >= grid(0).length || y < 0 || y >= grid.length

		if outOfBounds then false else
			val matches = grid(y)(x) == goal.charAt(acc)
			val next = (curr._1 + dir._1, curr._2 + dir._2)
			matches && _xmas(grid, next, dir, acc + 1, goal)

def xmas(grid: Vector[Vector[Char]], start: (Int, Int), goal: String): Int =
	dirs.filter(dir => _xmas(grid, start, dir, 0, goal)).length

val diagDirs = List((1, 1), (1, -1), (-1, 1), (-1, -1))

def xmas2(grid: Vector[Vector[Char]], start: (Int, Int), goal: String): List[(Int, Int)] =
	diagDirs.filter(dir => _xmas(grid, start, dir, 0, goal))
	.map(dir => (start._1 + dir._1, start._2 + dir._2))

@main
def day4() =
	val input = fromFile("input.txt").getLines().map(_.toVector).toVector

	val part1 = (for
		x <- 0 until input.length
		y <- 0 until input.length
	yield xmas(input, (x, y), "XMAS")).sum

	val part2 = (for
		x <- 0 until input.length
		y <- 0 until input.length
	yield xmas2(input, (x, y), "MAS"))
		.filter(_.length > 0)
		.foldRight(Nil: List[(Int, Int)])(_ ::: _)
		.groupBy(identity)
		.filter(_._2.length > 1).size

	println((part1, part2))
