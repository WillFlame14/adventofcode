import scala.io.Source._

enum Dest:
	case ToOutput(x: Int)
	case ToBot(x: Int)

object Dest:
	def parse(d: String, v: Int): Dest =
		if d == "output" then ToOutput(v) else ToBot(v)

case class Bot(chips: List[Int], low: Option[Dest], high: Option[Dest]):
	def addChip(c: Int) =
		this.copy(chips = this.chips :+ c)

	def clearChips =
		this.copy(chips = List())

	def setInst(l: Dest, h: Dest) =
		this.copy(low = Some(l), high = Some(h))

object Bot:
	def base = Bot(List(), None, None)

case class Factory(botMap: Map[Int, Bot], outputs: Map[Int, Int])

object Factory:
	def empty = Factory(Map(), Map())

val assignPattern = """value (\d+) goes to bot (\d+)""".r
val instPattern = """bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)""".r

extension (f: Factory)
	def perform(s: String): Factory =
		s match
			case instPattern(b, ld, lv, hd, hv) =>
				val low = Dest.parse(ld, lv.toInt)
				val high = Dest.parse(hd, hv.toInt)
				val bot = f.botMap.getOrElse(b.toInt, Bot.base).setInst(low, high)
				f.copy(botMap = f.botMap.updated(b.toInt, bot)).update(b.toInt)

			case assignPattern(v, b) =>
				val bot = f.botMap.getOrElse(b.toInt, Bot.base).addChip(v.toInt)
				f.copy(botMap = f.botMap.updated(b.toInt, bot)).update(b.toInt)

	def update(b: Int): Factory =
		f.botMap.get(b) match
			case Some(bot) if bot.chips.length == 2 => (bot.low, bot.high) match
				case (Some(l), Some(h)) =>
					val (x, y) = (bot.chips(0), bot.chips(1))

					// Part 1
					if bot.chips.contains(61) && bot.chips.contains(17) then println(b) else ()

					val (low, high) = if x < y then (x, y) else (y, x)
					val res = f.distribute(low, l).distribute(high, h)
					res.copy(botMap = res.botMap.updated(b, bot.clearChips))
				case _ => f
			case _ => f

	def distribute(a: Int, d: Dest): Factory =
		d match
			case Dest.ToOutput(x) => f.copy(outputs = f.outputs.updated(x, a))
			case Dest.ToBot(x) =>
				val bot = f.botMap.getOrElse(x, Bot.base).addChip(a)
				f.copy(botMap = f.botMap.updated(x, bot)).update(x)

@main
def day10() =
	val input = fromFile("input.txt").getLines()

	val factory = input.foldLeft(Factory.empty)((a, c) => a.perform(c))
	val outputs = factory.outputs.toList.sortBy(_._1).map(_._2)

	val part2 = outputs.slice(0, 3).product

	println(part2)
