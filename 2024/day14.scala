import scala.io.Source._
import aoc._

val DIMS = (101, 103)

case class Robot(pos: Pair[Int], vel: Pair[Int]):
	def update() =
		val (x, y) = pos + vel
		this.copy(pos = ((x + DIMS._1) % DIMS._1, (y + DIMS._2) % DIMS._2))

val robotPattern = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r

def parseRobot(s: String) =
	s match
		case robotPattern(px, py, vx, vy) => Robot((px.toInt, py.toInt), (vx.toInt, vy.toInt))

def quadrant(p: Pair[Int]): Option[Int] =
	p match
		case (x, y) if x < DIMS._1 / 2 =>
			if y < DIMS._2 / 2 then Some(2) else if y > DIMS._2 / 2 then Some(3) else None
		case (x, y) if x > DIMS._1 / 2 =>
			if y < DIMS._2 / 2 then Some(1) else if y > DIMS._2 / 2 then Some(4) else None
		case _ => None

def safetyFactor(rs: List[Robot]) =
	rs.foldRight(List(0, 0, 0, 0))((c, a) =>
		val q = quadrant(c.pos)
		if q.isDefined then a.updated(q.get - 1, a(q.get - 1) + 1) else a
	).product

def display(i: Int, rs: List[Robot]) =
	val grid = rs.foldRight(Vector.fill(DIMS._2)(Vector.fill(DIMS._1)('.')))((c, a) =>
		a.updated(c.pos._2, a(c.pos._2).updated(c.pos._1, '#')))
	println(grid.map(_.mkString + "\n").mkString + s"-----${i + 1}-----")

@main
def day14() =
	val input = fromFile("input.txt").getLines()
	val robots = input.map(parseRobot).toList

	val res = (0 until 100).foldRight(robots)((_, a) => a.map(_.update()))
	val part1 = safetyFactor(res)
	println(part1)

	// First, just display everything with a 200ms sleep and identify a recurring pattern where most of the rbots seem to be forming something.
	// One you find the pattern, then only display those ones with a 500ms sleep. The tree will become obvious.
	(0 until 10000).foldLeft(robots)((a, c) =>
		val next = a.map(_.update())
		if c % 101 == 9 then { display(c, next); Thread.sleep(500) } else ()
		next
	)
