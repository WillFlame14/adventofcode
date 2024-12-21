package day21

import scala.io.Source._
import aoc._

type Path = List[Char]

val numpad = Map(
	((0, 0), '7'), ((1, 0), '8'), ((2, 0), '9'),
	((0, 1), '4'), ((1, 1), '5'), ((2, 1), '6'),
	((0, 2), '1'), ((1, 2), '2'), ((2, 2), '3'),
				   ((1, 3), '0'), ((2, 3), 'A'))
val numpadR = numpad.map(_.swap)

val dirMap = Map(('^', (0, -1)), ('>', (1, 0)), ('v', (0, 1)), ('<', (-1, 0)))
val dirMapR = dirMap.map(_.swap)

val dirpad = Map(
				   ((1, 0), '^'), ((2, 0), 'A'),
	((0, 1), '<'), ((1, 1), 'v'), ((2, 1), '>'))
val dirpadR = dirpad.map(_.swap)

def traverseNumpad(start: Char, finish: Char): List[Path] =
	val paths = allShortest((3, 4), numpadR(start), numpadR(finish), Set((0, 3)))

	paths.map(pa => pa.zipWithIndex.tail.map(pi =>
		val (p, i) = pi
		dirMapR(p - pa(i - 1))) :+ 'A')

def traverseDirpad(start: Char, finish: Char): List[Path] =
	val paths = allShortest((3, 2), dirpadR(start), dirpadR(finish), Set((0, 0)))

	val x = paths.map(pa => pa.zipWithIndex.tail.map(pi =>
		val (p, i) = pi
		dirMapR(p - pa(i - 1))) :+ 'A')

	// Prefer >>^ and v<< over >^> and <v<
	x.filter(pa => pa.length < 4 || pa(0) == pa(1) || pa(1) == pa(2))

def getPaths(path: List[Char], traverse: (Char, Char) => List[Path]) =
	path.foldLeft(('A', Nil: List[Path]))((a, c) =>
		val (lastChar, list) = a
		val currPaths = traverse(lastChar, c)

		val nextPaths = if currPaths.isEmpty then list.map(_ :+ 'A') else
			currPaths.flatMap(pa => if list.isEmpty then List(pa) else list.map(l => l :++ pa))
		(c, nextPaths)
	)._2

lazy val getPathMemo: ((Char, Char, Int)) => Long = memoize(x =>
	val (start, finish, i) = x
	val currPaths = traverseDirpad(start, finish)

	if i == 0 then
		if currPaths.isEmpty then 1 else currPaths.map(_.length).min
	else
		if currPaths.isEmpty then
			getPathMemo(('A', 'A', i - 1))
		else
			currPaths.map(currPath =>
				currPath.zipWithIndex.foldLeft(0L)((a, c) =>
					val (cChar, ci) = c
					a + getPathMemo((if ci == 0 then 'A' else currPath(ci - 1), cChar, i - 1)))
			).min
)

def filterShortest[A](xs: List[List[A]]) =
	val shortest = xs.map(_.length).min
	xs.filter(_.length == shortest)

def press(str: String, robots: Int) =
	val r1Paths = filterShortest(getPaths(str.toList, traverseNumpad))

	val paths = (0 until robots).foldRight(r1Paths)((c, a) =>
		filterShortest(a.flatMap(path => getPaths(path, (s, f) => traverseDirpad(s, f)))))

	paths(0).length

def press2(str: String, robots: Int) =
	val r1Paths = filterShortest(getPaths(str.toList, traverseNumpad))

	r1Paths.map(path =>
		path.zipWithIndex.foldLeft(0L)((a, c) =>
			val (cChar, ci) = c
			a + getPathMemo((if ci == 0 then 'A' else path(ci - 1), cChar, robots - 1)))
	).min

@main
def day20() =
	val input = fromFile("input.txt").getLines().toList

	val part1 = input.foldRight(0)((c, a) => a + press(c, 2) * c.init.toInt)
	val part2 = input.foldRight(0L)((c, a) => a + press2(c, 25) * c.init.toInt)
	println((part1, part2))
