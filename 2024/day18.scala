package day18

import scala.io.Source._
import aoc._
import scala.annotation.tailrec

val DIMS = (71, 71)

def shortestPath(start: Pair[Int], target: Pair[Int], obstacles: Set[Pair[Int]]) =
	@tailrec def traverse(frontier: List[(Pair[Int], List[Pair[Int]])], visited: Set[Pair[Int]]): Option[List[Pair[Int]]] =
		if frontier.isEmpty then None else
			val (curr, path) = frontier.head
			val validNeighbours = neighbours(DIMS, curr).filter(p => !visited.contains(p))

			if validNeighbours.exists(_ == target) then Some(path :+ curr) else
				val newFrontier = frontier.tail ++ validNeighbours.zip(List.fill(4)(path :+ curr))
				traverse(newFrontier, visited ++ validNeighbours)

	traverse(List((start, Nil)), obstacles + start)

def firstFail(start: Pair[Int], target: Pair[Int], bytes: List[Pair[Int]]) =
	@tailrec def attempt(i: Int, cPath: List[Pair[Int]], cObstacles: Set[Pair[Int]]): Pair[Int] =
		val newObstacles = cObstacles + bytes(i)
		if !cPath.exists(_ == bytes(i)) then
			attempt(i + 1, cPath, newObstacles)
		else
			val newPath = shortestPath(start, target, newObstacles)
			if newPath.isDefined then
				attempt(i + 1, newPath.get, newObstacles)
			else
				bytes(i)

	val obstacles = bytes.take(1024).toSet
	attempt(1024, shortestPath(start, target, obstacles).get, obstacles)

@main
def day18() =
	val input = fromFile("input.txt").getLines()

	val bytes = input.foldRight(Nil)((c, a) =>
		val parts = c.split(",").map(_.toInt)
		val pair = (parts(0), parts(1))
		pair :: a)

	val part1 = shortestPath((0, 0), (70, 70), bytes.take(1024).toSet)
	println(part1.get.length)

	// val part2 = (1025 until bytes.length).find(x => !shortestPath((0, 0), (70, 70), bytes.take(x).toSet).isDefined)
	val part2 = firstFail((0, 0), (70, 70), bytes)
	println(part2)
