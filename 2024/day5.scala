import scala.io.Source._
import scala.annotation.tailrec

def validUpdate(pages: List[Int], rules: List[(Int, Int)]) =
	!rules.exists(rule =>
		val (before, after) = rule
		pages.contains(before) && pages.contains(after) &&
		pages.indexOf(before) > pages.indexOf(after))

@main
def day5() =
	val input = fromFile("input.txt").getLines().toList
	val (rules_r, pages_r) = input.span(_ == "")

	val rules = rules_r.foldRight(Nil)((c, a) =>
		val xs = c.split("\\|").map(_.toInt)
		(xs(0), xs(1)) :: a)

	val pages = pages_r.tail.foldRight(Nil)((c, a) =>
		c.split(",").map(_.toInt).toList :: a)

	val (valid, invalid) = pages.partition(p => validUpdate(p, rules))

	val part1 = valid.map(xs => xs(xs.length / 2)).sum

	val part2 = invalid.map(p =>
		val sorted = p.sortWith((a, b) =>
			val rule = rules.find(r => (a == r._1 && b == r._2) || (a == r._2 && b == r._1))
			rule.map(a == _._1).getOrElse(false)
		)
		sorted(sorted.length / 2)).sum

	println((part1, part2))
