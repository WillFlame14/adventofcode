package day17

import scala.io.Source._
import aoc._

case class Computer(a: Long, b: Long, c: Long):
	def combo(op: Int): Long =
		op match
			case 4 => a
			case 5 => b
			case 6 => c
			case x => x

	def perform(inst: Int, operand: Int): (Computer, Option[Int], Option[Int]) =
		inst match
			case 0 => (this.copy(a = (a / Math.pow(2, combo(operand))).toLong), None, None)
			case 1 => (this.copy(b = b ^ operand), None, None)
			case 2 => (this.copy(b = combo(operand) % 8), None, None)
			case 3 => if a == 0 then (this, None, None) else (this, Some(operand), None)
			case 4 => (this.copy(b = b ^ c), None, None)
			case 5 => (this, None, Some((combo(operand) % 8).toInt))
			case 6 => (this.copy(b = (a / Math.pow(2, combo(operand))).toLong), None, None)
			case 7 => (this.copy(c = (a / Math.pow(2, combo(operand))).toLong), None, None)

def runProgram(comp: Computer, program: List[Int]) =
	def execute(cComp: Computer, i: Int, output: List[Int]): List[Int] =
		if i >= program.length then output else
			val (newComp, jump, newOutput) = cComp.perform(program(i), program(i + 1))
			execute(newComp, jump.getOrElse(i + 2), newOutput.map(output :+ _).getOrElse(output))

	execute(comp, 0, Nil)

def testProgram(comp: Computer, program: List[Int], target: Int): Boolean =
	def execute2(cComp: Computer, i: Int): Boolean =
		if i >= program.length then false else
			val (newComp, jump, output) = cComp.perform(program(i), program(i + 1))
			if output.isDefined then output.map(_ == target).getOrElse(false) else
				execute2(newComp, jump.getOrElse(i + 2))

	execute2(comp, 0)

def findMatch(program: List[Int], target: List[Int], acc: Long = 0L): List[Long] =
	if target == Nil then List(acc) else
		val poss = (0 until 8)
			.map(_ + acc * 8)
			.filter(a =>
				testProgram(Computer(a, 0, 0), program, target.head))
		poss.flatMap(x => findMatch(program, target.tail, x)).toList

@main
def day17() =
	val program = List(
		2,4,	// b = (a % 8)
		1,1,	// b = b xor 1
		7,5,	// c = a / 2^b
		0,3,	// a = a / 2^3
		1,4,	// b = b xor 4
		4,4,	// b = b xor c
		5,5,	// print(b % 8)
		3,0)	// jump to beginning if a != 0

	val computer = Computer(30886132, 0, 0)
	val part1 = runProgram(computer,program).mkString(",")
	val part2 = findMatch(program, program.reverse)(0)

	println(part1)
	println(part2)
