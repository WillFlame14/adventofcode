package day23

import scala.io.Source._
import aoc._

def buildLAN(strs: List[String]) =
	strs.foldRight(Map(): Map[String, Set[String]])((c, a) =>
		val Array(x, y) = c.split("-")
		a.updated(x, a.getOrElse(x, Set()) + y).updated(y, a.getOrElse(y, Set()) + x)
	)

def conn3(mapLAN: Map[String, Set[String]]) =
	val keys = mapLAN.keys.toList
	(for
		a <- 0 until mapLAN.size
		b <- (a + 1 until mapLAN.size) if mapLAN(keys(a)).contains(keys(b))
		c <- (b + 1 until mapLAN.size) if mapLAN(keys(a)).contains(keys(c)) && mapLAN(keys(c)).contains(keys(b))
	yield (keys(a), keys(b), keys(c))).toList

def largestConn(mapLAN: Map[String, Set[String]]) =
	val keys = mapLAN.keys.toList

	def attempt(existing: Set[String], prospect: String) =
		existing.forall(x => mapLAN(x).contains(prospect) && mapLAN(prospect).contains(x))

	def loop(groups: List[Set[String]], i: Int): List[Set[String]] =
		if i == keys.length then groups else
			val newGroups = groups :++ groups.filter(g => attempt(g, keys(i))).map(_ + keys(i))

			loop(newGroups, i + 1)

	val largestGroup = (0 until keys.size - 1).map(i =>
		loop(List(Set(keys(i))), i + 1).maxBy(_.size)).maxBy(_.size)

	largestGroup.toList.sorted.mkString(",")

@main
def day23() =
	val input = fromFile("input.txt").getLines().toList

	val mapLAN = buildLAN(input)
	val conn3s = conn3(mapLAN)

	val part1 = conn3s.filter(x =>
		val (a, b, c) = x
		List(a, b, c).exists(_.startsWith("t"))).length
	println(part1)

	val part2 = largestConn((mapLAN))
	println(part2)
