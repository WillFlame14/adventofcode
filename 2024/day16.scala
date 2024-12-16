package day16

import scala.io.Source._
import aoc._
import scala.annotation.tailrec

def dirs = List((0, -1), (1, 0), (0, 1), (-1, 0))

type MMap = Map[(Point[Int], Point[Int]), (Int, Set[Point[Int]])]

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

@tailrec def dp(grid: Grid[Char], frontier: MMap, best: MMap, target: Point[Int]): MMap =
	if frontier.size == 0 then best else
		val dims = (grid(0).length, grid.length)

		val (newFrontier, newBest) = frontier.foldRight((Map(): MMap, best))((c, a) =>
			val (aFrontier, aBest) = a
			val ((point, dir), (score, visited)) = c
			val dirI = dirs.indexOf(dir)

			val validNeighbours: List[((Point[Int], Point[Int]), (Int, Set[Point[Int]]))] = List(
				((point + dir, dir), (score + 1, visited + point)),
				((point, dirs((dirI + 1) % dirs.length)), (score + 1000, visited)),
				((point, dirs((dirI - 1 + dirs.length) % dirs.length)), (score + 1000, visited)))

			validNeighbours.foldRight((aFrontier, aBest))((c, a) =>
				val (cFrontier, cBest) = a
				val (pos, res @ (sc, vis)) = c

				if gridAt(grid, pos._1) == '#' then
					a
				else if !cBest.contains(pos) then
					(cFrontier.updated(pos, res), cBest.updated(pos, res))
				else
					val (prevScore, prevVisited) = cBest(pos)
					if sc < prevScore then
						(cFrontier.updated(pos, res), cBest.updated(pos, res))
					else if sc == prevScore then
						val newRes = (sc, vis ++ prevVisited)
						(cFrontier.updated(pos, newRes), cBest.updated(pos, newRes))
					else
						a

			)
		)
		dp(grid, newFrontier, newBest, target)


@main
def day16() =
	val input = fromFile("input.txt").getLines().toList

	val (grid, start, finish) = parse(input)
	println((start, finish))

	val finished = dp(grid, Map(((start, (1, 0)), (0, Set(start)))), Map(((start, (1, 0)), (0, Set(start)))), finish)
		.filter(_._1._1 == finish)
	val part1 = finished.map(_._2._1).min
	println(part1)

	val part2 = finished.filter(_._2._1 == part1).map(_._2._2).foldRight(Set())(_ ++ _).size + 1
	println(part2)
