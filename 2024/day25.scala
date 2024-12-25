package day25

import scala.io.Source._
import aoc._

def parse(strs: List[String]) =
	if strs.head.head == '.' then
		(strs.map(_.toList).transpose.map(_.dropWhile(_ == '.').length), "key")
	else
		(strs.map(_.toList).transpose.map(_.takeWhile(_ == '#').length), "lock")

def fit(key: List[Int], lock: List[Int]) =
	key.zip(lock).map(_ + _).forall(_ <= 7)

@main
def day25() =
	val input = fromFile("input.txt").getLines().toList

	val items = input.grouped(8).map(_.init).map(parse).toList
	val (keys, locks) = items.partition(_._2 == "key")

	val part1 = (for
		k <- keys
		l <- locks if fit(k._1, l._1)
	yield (k, l)).length

	println(part1)
