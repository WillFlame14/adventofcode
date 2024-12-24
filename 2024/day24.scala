package day23

import scala.io.Source._
import aoc._

type InputMap = Map[String, Boolean]
type GateMap = Map[String, (String, String, String)]

enum Wire:
	case Input(s: String)
	case Gate(l: Wire, op: String, r: Wire)

def parseInputs(strs: List[String]) =
	strs.foldRight(Map(): InputMap)((c, a) =>
		val Array(name, bit) = c.split(": ")
		a.updated(name, bit == "1"))

val gatePattern = """(\w+) (AND|OR|XOR) (\w+) -> (\w+)""".r

def parseGates(strs: List[String]) =
	strs.foldRight(Map(): GateMap)((c, a) => c match
		case gatePattern(l, op, r, out) =>
			a.updated(out, (l, op, r))
	)

def getOutput(gate: String, inputs: InputMap, gates: GateMap): (Boolean, InputMap) =
	if inputs.contains(gate) then (inputs(gate), inputs) else
		val (l, op, r) = gates(gate)
		val (lv, iMap1) = getOutput(l, inputs, gates)
		val (rv, iMap2) = getOutput(r, iMap1, gates)

		val res = op match
			case "AND" => lv && rv
			case "OR" => lv || rv
			case "XOR" => lv ^ rv

		(res, iMap2.updated(gate, res))

lazy val getConstruct: ((String, InputMap, GateMap)) => Wire = memoize(x =>
	val (gate, inputs, gates) = x
	if inputs.contains(gate) then Wire.Input(gate) else
		val (l, op, r) = gates(gate)
		println(s"$gate is $l $op $r")
		val lv = getConstruct(l, inputs, gates)
		val rv = getConstruct(r, inputs, gates)

		Wire.Gate(lv, op, rv)
)

def pairMatch[A](x: A, y: A, fx: A => Boolean, fy: A => Boolean) =
	if fx(x) then fy(y)
	else if fy(x) then fx(y)
	else false

def isBasicGate(wire: Wire, exOp: String, i: Int): Boolean =
	wire match
		case Wire.Input(_) => false
		case Wire.Gate(l, op, r) =>
			op == exOp && pairMatch(l, r, _ == Wire.Input(f"x${i}%02d"), _ == Wire.Input(f"y${i}%02d"))

val nextOp = Map(("XOR", "OR"), ("OR", "AND"), ("AND", "OR"))
val basicOp = Map(("XOR", "XOR"), ("OR", "AND"), ("AND", "XOR"))

def isNestedGate(wire: Wire, exOp: String, i: Int): Boolean =
	if i == 0 then isBasicGate(wire, exOp, i) else
		wire match
			case Wire.Input(_) => false
			case Wire.Gate(l, op, r) =>
				val nextI = if exOp == "OR" then i else i - 1
				exOp == op && pairMatch(l, r,
					x => isBasicGate(x, basicOp(exOp), i),
					x => isNestedGate(x, if nextI == 0 then "AND" else nextOp(exOp)
				, nextI))

def isCorrect(wire: Wire, z: Int, inputs: InputMap, gates: GateMap) =
	if z == 0 then isBasicGate(wire, "XOR", 0) else isNestedGate(wire, "XOR", z)

@main
def day24() =
	val input = fromFile("input.txt").getLines().toList
	val (rawInputs, rawGates) = input.splitAt(input.indexOf(""))

	val inputs = parseInputs(rawInputs)
	val gates = parseGates(rawGates.tail)

	val zGates = gates.keySet.filter(_.startsWith("z")).toList.sortBy(_.tail.toInt)
	val (part1, _) = zGates.foldRight((0L, inputs))((c, a) =>
		val (av, aInputs) = a
		val (cv, cInputs) = getOutput(c, aInputs, gates)
		(av * 2 + (if cv then 1 else 0), cInputs)
	)
	println(part1)

	// This is a better way of determining which ones you need to modify. I overcomplicated it.
	val x = (0 until 45).foldRight(0L)((c, a) => a * 2 + (if inputs(f"x${c}%02d") then 1 else 0))
	val y = (0 until 45).foldRight(0L)((c, a) => a * 2 + (if inputs(f"y${c}%02d") then 1 else 0))
	// println((part1 - (x + y)).toBinaryString)

	// Use the first one to determine which is the first incorrect one. Then use the 2nd one to manually check and fix the connections.
	// println(zGates.map(z => isCorrect(getConstruct(z, inputs, gates), z.tail.toInt, inputs, gates)).zipWithIndex)
	// println(isCorrect(getConstruct("z20", inputs, gates), 20, inputs, gates))

	val part2 = List("z09", "rkf", "z24", "vcg", "rvc", "rrs", "z20", "jgb").sorted.mkString(",")
	println(part2)
