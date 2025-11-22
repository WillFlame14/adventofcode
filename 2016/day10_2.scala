//> using dep "org.typelevel::cats-core::2.12.0"
import cats.data.State
import day10.*

import scala.io.Source.*

case class BotAction(
	botId: Int,
	chips: (Int, Int),
	lowDest: Dest,
	highDest: Dest
)

case class FactoryH(
	factory: Factory,
	history: List[BotAction] = Nil
):
	def perform(s: String): FactoryH =
		s match
			case instPattern(b, ld, lv, hd, hv) =>
				val low = Dest.parse(ld, lv.toInt)
				val high = Dest.parse(hd, hv.toInt)
				val bot = factory.botMap.getOrElse(b.toInt, Bot.base).setInst(low, high)
				copy(factory = factory.copy(botMap = factory.botMap.updated(b.toInt, bot))).update(b.toInt)

			case assignPattern(v, b) =>
				val bot = factory.botMap.getOrElse(b.toInt, Bot.base).addChip(v.toInt)
				copy(factory = factory.copy(botMap = factory.botMap.updated(b.toInt, bot))).update(b.toInt)

	def update(b: Int): FactoryH =
		factory.botMap.get(b) match
			case Some(bot) if bot.chips.length == 2 => (bot.low, bot.high) match
				case (Some(l), Some(h)) =>
					val (x, y) = (bot.chips(0), bot.chips(1))
					val (low, high) = if x < y then (x, y) else (y, x)

					// Record the action before performing it
					val action = BotAction(b, (low, high), l, h)

					FactoryH(
						factory.copy(botMap = factory.botMap.updated(b, bot.clearChips)),
						action :: history
					).distribute(low, l).distribute(high, h)
				case _ => this
			case _ => this

	def distribute(a: Int, d: Dest): FactoryH =
		d match
			case Dest.ToOutput(x) => copy(factory = factory.copy(outputs = factory.outputs.updated(x, a)))
			case Dest.ToBot(x) =>
				val bot = factory.botMap.getOrElse(x, Bot.base).addChip(a)
				copy(factory = factory.copy(botMap = factory.botMap.updated(x, bot))).update(x)

enum Inst:
	case GiveVal(i: Int, v: Int)
	case AssignDest(i: Int, l: Dest, h: Dest)

val _assignPattern = """value (\d+) goes to bot (\d+)""".r
val _instPattern = """bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)""".r

object Inst:
	def parse(s: String): Inst =
		s match
			case instPattern(b, ld, lv, hd, hv) =>
				val low = Dest.parse(ld, lv.toInt)
				val high = Dest.parse(hd, hv.toInt)
				AssignDest(b.toInt, low, high)

			case assignPattern(v, b) =>
				GiveVal(b.toInt, v.toInt)

extension[S,A] (s: State[S, A])
	def map2[B,C](t: State[S, B])(f: (A, B) => C) =
		for
			a <- s
			b <- t
		yield f(a, b)

def traverse[S,A,B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
	as.foldRight(State(x => (x, Nil)): State[S, List[B]])((c, a) => f(c).map2(a)(_ :: _))

object Work:
	def simulate(inputs: List[Inst]): State[FactoryH, (Int, Map[Int, Int])] =
		for
			_ <- traverse(inputs)(i => State.modify(updateD(i)))
			botWith17And61 <- findBotComparing(17, 61)
			s <- State.get
		yield (botWith17And61, s.factory.outputs)

	def findBotComparing(chip1: Int, chip2: Int): State[FactoryH, Int] =
		State.inspect(_.history.find(action =>
				val chips = Set(action.chips._1, action.chips._2)
				chips == Set(chip1, chip2)
			)
			.map(_.botId)
			.getOrElse(-1))

	def updateD = (i: Inst) => (f: FactoryH) =>
		i match
			case Inst.AssignDest(i, low, high) =>
				val bot = f.factory.botMap.getOrElse(i, Bot.base).setInst(low, high)
				f.copy(factory = f.factory.copy(botMap = f.factory.botMap.updated(i, bot)))
				.update(i)

			case Inst.GiveVal(i, v) =>
				val bot = f.factory.botMap.getOrElse(i, Bot.base).addChip(v.toInt)
				f.copy(factory = f.factory.copy(botMap = f.factory.botMap.updated(i, bot)))
				.update(i)

@main
def day10_2() =
	val input = fromFile("input.txt").getLines()
	val (part1, outputs) = Work.simulate(input.map(x => Inst.parse(x)).toList)
		.runA(FactoryH(Factory(Map(), Map()), Nil)).value

	val part2 = outputs.toList.sortBy(_._1).map(_._2).slice(0, 3).product

	println((part1, part2))
