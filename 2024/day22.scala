package day22

import scala.io.Source._
import aoc._

def mix(a: Long, b: Long): Long =
	a ^ b

def prune(a: Long): Long =
	a % 16777216

def secret(x: Long) =
	val a = prune(mix(x, (x * 64)))
	val b = prune(mix(a, (a / 32)))
	val c = prune(mix(b, b * 2048))
	c

def genChanges(start: Long) =
	(0 until 2001).foldRight((start, Nil: List[Long], 0L, Map(): Map[String, Long]))((_, a) =>
		val (n, changes, last, map) = a
		if n == start && changes == Nil then
			(secret(n), changes, n, map)
		else
			val res = changes :+ ((n % 10) - (last % 10))
			val newChanges = if res.length > 4 then res.drop(res.length - 4) else res

			val newMap = if newChanges.length == 4 && !map.contains(newChanges.mkString) then map.updated(newChanges.mkString, n % 10) else map
			(secret(n), newChanges, n, newMap)
		)._4

def bestChanges(changes: List[Map[String, Long]]) =
	val allChanges = changes.flatMap(_.keySet).foldRight(Set(): Set[String])((c, a) => a + c)

	def score(change: String) =
		changes.map(c => c.get(change)).foldRight(0L)((c, a) => c.map(_ + a).getOrElse(a))

	allChanges.map(change => (change, score(change))).foldRight(("", -1L))((c, a) => if c._2 >= a._2 then c else a)

@main
def day22() =
	val input = fromFile("input.txt").getLines().toList

	val part1 = (0 until 2000).foldRight(input.map(_.toLong))((_, a) =>
		a.map(x => secret(x))).sum

	val changes = input.map(i => genChanges(i.toLong))
	val part2 = bestChanges(changes)._2

	println((part1, part2))
