package day17

import scala.io.Source._
import aoc._

val clayPattern = """(x|y)=(\d+), (x|y)=(\d+)..(\d+)""".r

/** Returns a list of pairs that represent the wall. */
def parse(s: String): Iterable[Pair[Int]] = s match
	case clayPattern(a, l, _, s, t) =>
		(s.toInt to t.toInt).map(c =>
			if a == "x" then (l.toInt, c) else (c, l.toInt))

/** Returns the potential locations the water block could flow to. */
def flow(water: Pair[Int], stillW: Set[Pair[Int]], clay: Set[Pair[Int]], bounds: Pair[Int]): Set[Pair[Int]] =
	val (x, y) = water
	val below = (x, y + 1)
	val neighbours = if clay.contains(below) || stillW.contains(below) then
		List((x + 1, y), (x - 1, y)) else List(below)

	neighbours.filter(p => !clay.contains(p) && p(1) <= bounds(1)).toSet

/** Returns the new locations of all water blocks, after flowing. */
def flowAll(water: Set[Pair[Int]], stillW: Set[Pair[Int]], clay: Set[Pair[Int]], bounds: Pair[Int]): Set[Pair[Int]] =
	water.foldRight(Set(): Set[Pair[Int]])((w, a) =>
		val flowed = flow(w, stillW, clay, bounds)
		a.union(flowed))

/** Returns the locations of the flowing and settled water blocks after no new water flows in range. */
def flowUntil(source: Pair[Int], clay: Set[Pair[Int]], settleable: Iterable[Iterable[Pair[Int]]]): (Set[Pair[Int]], Set[Pair[Int]]) =
	val ys = clay.map(c => c(1))
	val bounds = (ys.min, ys.max)

	def reflow(water: Set[Pair[Int]], stillW: Set[Pair[Int]], i: Int): (Set[Pair[Int]], Set[Pair[Int]]) =
		val flowed = flowAll(water, stillW, clay, bounds)
		val newSettled = settle(water, stillW)
		val nextW = flowed -- newSettled + source
		if nextW == water then (water, stillW) else reflow(nextW, stillW.union(newSettled), i + 1)

	def settle(water: Set[Pair[Int]], stillW: Set[Pair[Int]]) =
		settleable.filter(ps =>
			ps.forall(p =>
				val below = p + (0, 1)
				water.contains(p) && (stillW.contains(below) || clay.contains(below)))).flatten.toSet

	val (water, stillW) = reflow(Set(source), Set(), 0)
	// display(water, stillW, clay)
	(water, stillW)

/** Returns a list of list of pairs: each list contains the set of points that, if filled with water and supported directly underneath, will become settled. */
def findSettleable(clay: List[Pair[Int]]): Iterable[Iterable[Pair[Int]]] =
	val ys = clay.map(p => p(1))
	val groups = clay.groupBy(p => p(1)).map((y, xs) => (y, xs.map(p => p(0)).sorted))

	groups.flatMap((y, xs) =>
		xs.zip(xs.tail).map((l, r) =>
			(l + 1 until r).map(x => (x, y))))

def reachable(water: Set[Pair[Int]], minY: Int) =
	water.filter(p => p(1) >= minY).size

@main
def day17() =
	val input = fromFile("input.txt").getLines()
	val clay = input.flatMap(parse).toList

	val settleable = findSettleable(clay)
	val (water, stillW) = flowUntil(((500, 0)), clay.toSet, settleable)

	val minY = clay.map(p => p(1)).min
	val part1 = reachable(water, minY) + reachable(stillW, minY)
	val part2 = reachable(stillW, minY)
	println((part1, part2))

def display(water: Set[Pair[Int]], stillW: Set[Pair[Int]], clay: Set[Pair[Int]]) =
	val (xs, ys) = clay.toList.unzip
	val bounds = ((xs.min, xs.max), (ys.min, ys.max))

	println((ys.min to ys.max).map(y =>
		(xs.min to xs.max).map(x =>
			val point = (x, y)
			if water.contains(point) then "|"
			else if stillW.contains(point) then "~"
			else if clay.contains(point) then "#"
			else "."
		).mkString
	).mkString("\n"))
