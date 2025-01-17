package aoc
import Numeric.Implicits._
import scala.annotation.tailrec
import scala.collection.mutable.HashMap

type Pair[A] = Tuple2[A, A]

val cardinalDirs = List((0, -1), (1, 0), (0, 1), (-1, 0))

extension[A: Numeric] (p: Pair[A])
	def +(p2: Pair[A]) =
		(p._1 + p2._1, p._2 + p2._2)

	def -(p2: Pair[A]) =
		(p._1 - p2._1, p._2 - p2._2)

	def abs =
		val num = implicitly[Numeric[A]]
		num.abs(p._1) + num.abs(p._2)

	def inBounds(dims: (Int, Int)) =
		val num = implicitly[Numeric[A]]
		num.gteq(p._1, num.fromInt(0)) && num.lt(p._1, num.fromInt(dims._1)) && num.gteq(p._2, num.fromInt(0)) && num.lt(p._2, num.fromInt(dims._2))

type Grid[A] = List[List[A]]

def gridAt[A,B <: Int](grid: Grid[A], p: Pair[B]) =
	val (x, y) = p
	grid(y.toInt)(x.toInt)

def updateAt[A,B <: Int](grid: Grid[A], p: Pair[B], v: A) =
	val (x, y) = p
	grid.updated(y.toInt, grid(y).updated(x.toInt, v))

def neighbours[A <: Int](dims: (Int, Int), p: Pair[A]) =
	cardinalDirs.map(_ + p).filter(_.inBounds(dims))

def displayGrid[A](grid: Grid[A], p: Pair[Int]) =
	println(updateAt(grid, p, '@').map(_.mkString + "\n").mkString)

def shortestPath(dims: (Int, Int), start: Pair[Int], target: Pair[Int], obstacles: Set[Pair[Int]]) =
	@tailrec def traverse(frontier: List[(Pair[Int], List[Pair[Int]])], visited: Set[Pair[Int]]): Option[List[Pair[Int]]] =
		if frontier.isEmpty then None else
			val (curr, path) = frontier.head
			val validNeighbours = neighbours(dims, curr).filter(p => !visited.contains(p))

			if validNeighbours.exists(_ == target) then Some(path :+ curr :+ target) else
				val newFrontier = frontier.tail ++ validNeighbours.zip(List.fill(4)(path :+ curr))
				traverse(newFrontier, visited ++ validNeighbours)

	traverse(List((start, Nil)), obstacles + start)

def allShortest(dims: (Int, Int), start: Pair[Int], target: Pair[Int], obstacles: Set[Pair[Int]]) =
	@tailrec def traverse(frontier: List[(Pair[Int], List[Pair[Int]])], visited: Set[Pair[Int]], paths: List[List[Pair[Int]]] = Nil): List[List[Pair[Int]]] =
		if frontier.isEmpty then paths else
			val (curr, path) = frontier.head
			val validNeighbours = neighbours(dims, curr).filter(p => !visited.contains(p))

			val newPaths = if validNeighbours.exists(_ == target) then
				paths :+ (path :+ curr :+ target) else paths
			val newFrontier = frontier.tail ++ validNeighbours.zip(List.fill(4)(path :+ curr))
			traverse(newFrontier, visited + curr, newPaths)

	traverse(List((start, Nil)), obstacles + start)

def memoize[I, O](f: I => O): I => O = new HashMap[I, O]() {self =>
  override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
}
