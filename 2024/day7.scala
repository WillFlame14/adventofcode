import scala.io.Source._

def solve(target: Long, acc: Long, nums: List[Long], concat: Boolean = false): Boolean =
	if acc > target then false else
		nums match
			case Nil => acc == target
			case x :: xs =>
				solve(target, acc + x, nums.tail, concat) ||
				solve(target, acc * x, nums.tail, concat) ||
				(concat || solve(target, s"$acc$x".toLong, nums.tail, concat))

def parse(s: String): (Long, List[Long]) =
	val (test, rest) = s.splitAt(s.indexOf(":"))
	(test.toLong, rest.drop(2).split(" ").map(_.toLong).toList)

@main
def day7() =
	val input = fromFile("input.txt").getLines().toList

	val (part1, part2) = input.map(parse).foldRight((0, 0): (Long, Long))((c, a) =>
		val (target, nums) = c
		(if solve(target, nums(0), nums.tail) then target + a._1 else a._1,
			if solve(target, nums(0), nums.tail, true) then target + a._2 else a._2))

	println((part1, part2))
