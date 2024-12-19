package day19

import scala.io.Source._
import aoc._

type Cache = Map[String, Long]

def buildable(target: String, parts: List[String]): Boolean =
	if parts.exists(_ == target) then true else
		val starts = parts.filter(p => target.startsWith(p))
		starts.exists(s => buildable(target.slice(s.length, target.length), parts))

def buildableAll(target: String, parts: List[String], cache: Cache): (Long, Cache) =
	if cache.contains(target) then (cache(target), cache) else
		if target == "" then (1, cache) else
			val starts = parts.filter(p => target.startsWith(p))
			val (newV, newCache) = starts.foldRight((0L, cache))((c, a) =>
				val (accV, accC) = a
				val (currV, currC) = buildableAll(target.slice(c.length, target.length), parts, accC)
				(accV + currV, currC))
			(newV, newCache.updated(target, newV))

@main
def day19() =
	val input = fromFile("input.txt").getLines().toList
	val parts = input(0).split(", ").toList

	val part1 = input.drop(2).foldRight(0)((c, a) =>
		if buildable(c, parts) then a + 1 else a)

	val (part2, _) = input.drop(2).foldRight((0L, Map(): Cache))((c, a) =>
		val (v, ca) = a
		val (v2, ca2) = buildableAll(c, parts, ca)
		(v2 + v, ca2))

	println((part1, part2))
