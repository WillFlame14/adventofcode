import scala.io.Source._
import scala.annotation.tailrec

def expand(s: List[Int]) =
	s.zipWithIndex.flatMap(ss =>
		val (num, i) = ss
		if i % 2 == 0 then List.fill(num)(i / 2) else List.fill(num)(-1)
	)

def expand2(s: List[Int]) =
	s.zipWithIndex.map(ss =>
		val (num, i) = ss
		(if i % 2 == 0 then i / 2 else -1, num))

def expand3(s: List[Int]): (List[(Int, Int, Int)], List[(Int, Int, Int)]) =
	val (disks, spaces) = expand2(s).foldLeft((Nil: List[(Int, Int, Int)], 0))((a, c) =>
		val (acc, i) = a
		val (n, l) = c
		(acc :+ (i, n, l), i + l)
	)._1.zipWithIndex.partition(x => x._2 % 2 == 0)

	(disks.map(_._1), spaces.map(_._1))

@tailrec def shift(xs: List[Int], i: Int, j: Int): List[Int] =
	if i == j then xs else
		val (ci, cj) = (xs(i), xs(j))

		if ci != -1 then shift(xs, i + 1, j) else
			if cj == -1 then shift(xs, i, j - 1) else
				shift(xs.updated(i, cj).updated(j, -1), i + 1, j - 1)

@tailrec def shift3(disks: List[(Int, Int, Int)], spaces: List[(Int, Int, Int)], built: List[(Int, Int, Int)] = Nil): List[(Int, Int, Int)] =
	val disk @(index, num, len) = disks(disks.length - 1)
	if (index < spaces(0)._1) then disks ++: built else
		def loop(j: Int): (List[(Int, Int, Int)], (Int, Int, Int)) =
			val (sindex, snum, slen) = spaces(j)
			if sindex > index then (spaces, disk) else
				if len > slen then
					loop(j + 1)
				else
					val newSpaces =
						val (bef, aft) = spaces.splitAt(j)
						if len == slen then bef :++ aft.tail else (bef :+ (sindex + len, snum, slen - len)) ++: aft.tail
					(newSpaces, (sindex, num, len))

		val (newSpaces, newDisk) = loop(0)
		shift3(disks.init, newSpaces, built :+ newDisk)

@main
def day9() =
	val input = fromFile("input.txt").getLines().toList(0)
	val list = input.split("").map(_.toInt).toList
	val expandedList = expand(list)

	val shifted = shift(expandedList, 0, expandedList.length - 1).takeWhile(_ != -1)
	val part1: Long = shifted.zipWithIndex.map(x =>
		val (a, b) = x
		a * b
		).foldRight(0: Long)(_ + _)

	val (disks, spaces) = expand3(list)
	val shifted3 = shift3(disks, spaces).sortBy(_._1)
	val part2 = shifted3.foldRight(0: Long)((c, a) =>
		val (index, num, len) = c
		a + (index until index + len).map(_ * num).foldRight(0: Long)(_ + _)
	)

	println((part1, part2))
