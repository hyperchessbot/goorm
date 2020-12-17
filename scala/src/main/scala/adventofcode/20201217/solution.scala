package adventofcode20201217

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	var version:Int = 0
	
	case class Vect4(x:Int, y:Int, z:Int, w:Int){
		def +(v:Vect4):Vect4 = Vect4(x + v.x, y + v.y, z + v.z, w + v.w)
	}
		
	case class Grid(){
		val g = scala.collection.mutable.Map[Vect4, Boolean]()
		
		def read(lines:List[String]):Grid = {for(y <- 0 until lines.length; x <- 0 until lines(0).length)
			g.update(Vect4(x, y, 0, 0), lines(y).charAt(x) == '#'); this}
		
		def neighbours(v:Vect4):List[Vect4] = (for(x <- -1 until 2; y <- -1 until 2; z <- -1 until 2; w <- -1 * version until version + 1 if(!((x == 0)&&(y == 0)&&(z == 0)&&(w == 0)))) yield v + Vect4(x, y, z, w)).toList
		
		def active(v:Vect4):Boolean = if(g.contains(v)) g(v) else false
		
		def activeNeighbours(v:Vect4):Int = neighbours(v).count(active(_))
		
		def cloneFunc(v: Vect4):Tuple2[Vect4, Boolean] = {val ans = activeNeighbours(v); v -> (if(active(v)) ((ans == 2) || (ans == 3)) else (ans == 3))}
		
		override def clone():Grid = {val newG = Grid();	for((k, v) <- g; n <- neighbours(k) if(!newG.g.contains(n))) newG.g += cloneFunc(n); newG}
		
		def allActive:Int = g.values.count(identity)
		
		def repr(label:String):String = s"$label : all = ${g.size} , active = ${allActive}"
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {		
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		version = input._2
		
		var g = Grid().read(lines)
		
		println(g.repr("initial"))
		
		for(i <- 0 until 6){
			g = g.clone()
			
			println(g.repr(s"iteration ${i+1}"))
		}
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0), ("example", 1), ("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
