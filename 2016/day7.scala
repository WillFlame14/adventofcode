import scala.io.Source._

def abba(str: String) =
	str.length == 4 &&
	str(0) == str(3) &&
	str(1) == str(2) &&
	str(0) != str(1)

def has_abba(str: String) =
	(0 until str.length)
	.filter(_ + 4 <= str.length)
	.exists(i => abba(str.slice(i, i + 4)))

def supports_tls(str: String) =
	val (outerI, innerI) = str.split("[\\[\\]]").zipWithIndex.partition(_._2 % 2 == 0)
	!innerI.exists(x => has_abba(x._1)) && outerI.exists(x => has_abba(x._1))

def aba(str: String) =
	str.length == 3 &&
	str(0) == str(2) &&
	str(0) != str(1)

def all_aba(str: String) =
	(0 until str.length)
	.filter(_ + 3 <= str.length)
	.map(i => str.slice(i, i + 3))
	.filter(aba)

def supports_ssl(str: String) =
	val (outerI, innerI) = str.split("[\\[\\]]").zipWithIndex.partition(_._2 % 2 == 0)
	val outerABA = outerI.flatMap((s, i) => all_aba(s))
	val innerABA = innerI.flatMap((s, i) => all_aba(s))
	outerABA.exists(x => innerABA.exists(y => x(0) == y(1) && x(1) == y(2)))

@main
def main(): Unit =
	val input = fromFile("input.txt").getLines()

	val (p1, p2) = input.foldLeft((0, 0))((a, c) =>
		(if supports_tls(c) then a._1 + 1 else a._1,
			if supports_ssl(c) then a._2 + 1 else a._2))

	println((p1, p2))
