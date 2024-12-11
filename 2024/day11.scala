import scala.io.Source._

def _blink(stone: Long): List[Long] =
	stone match
		case 0 => List(1)
		case x if s"$stone".length % 2 == 0 =>
			val str = s"$stone"
			val (a, b) = str.splitAt(str.length / 2)
			List(a.toLong, b.toLong)
		case x => List(x * 2024)

def blinkR(stone: Long, i: Int, cache: Map[(Long, Int), Long]): (Long, Map[(Long, Int), Long]) =
	if cache.contains((stone, i)) then (cache((stone, i)), cache) else
		if i == 0 then (1, cache) else
			val newStones = _blink(stone)
			val (len, newCache) = newStones.foldRight((0: Long, cache))((c, a) =>
				val (al, aC) = a
				val (l, nC) = blinkR(c, i - 1, aC)
				(al + l, nC))
			(len, newCache.updated((stone, i), len))

@main
def day11() =
	val input = fromFile("input.txt").getLines().next
	val stones = input.split(" ").map(_.toLong).toList
	val part1 = (0 until 25).foldRight(stones)((_, a) => a.flatMap(_blink)).length

	println(part1)

	val (part2, _) = stones.foldRight((0: Long, Map(): Map[(Long, Int), Long]))((c, a) =>
		val (al, aC) = a
		val (len, cache) = blinkR(c, 75, aC)
		(al + len, cache))

	println(part2)
