package day18

import scala.io.Source._
import aoc._

def adjDirs = List((0, -1), (1, 0), (0, 1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

def update(grid: Grid[Char]): Grid[Char] =
	(0 until grid.length).map(y =>
		val line = grid(y)
		(0 until line.length).map(x =>
			val adjacent = adjDirs.map(d => gridAtOpt(grid, d + (x, y))).flatten
			line(x) match
				case '.' =>
					if adjacent.filter(_ == '|').length >= 3 then '|' else '.'
				case '|' =>
					if adjacent.filter(_ == '#').length >= 3 then '#' else '|'
				case '#' =>
					if adjacent.exists(_ == '#') && adjacent.exists(_ == '|') then '#' else '.'
			).toList).toList

def findCycle(grid: Grid[Char]) =
	def loop(currG: Grid[Char], currC: Map[String, Int], i: Int): (Int, Int) =
		val hash = gridToString(currG)
		if currC.contains(hash) then (currC(hash), i - currC(hash)) else
			val nextC = currC.updated(hash, i)
			loop(update(currG), nextC, i + 1)

	loop(grid, Map(), 0)

def resourceValue(grid: Grid[Char]) =
	val List(trees, lumberyards) = List('|', '#').map(c => grid.map(_.filter(_ == c).length).sum)
	trees * lumberyards

@main
def day18() =
	val input = fromFile("input.txt").getLines().toList
	val grid: Grid[Char] = input.map(line => line.toList)

	val part1 = resourceValue((0 until 10).foldLeft(grid)((a, _) => update(a)))

	val (offset, cycle) = findCycle(grid)
	val iterations = (1000000000 - offset) % cycle
	val part2 = resourceValue((0 until offset + iterations).foldLeft(grid)((a, _) => update(a)))

	println((part1, part2))
