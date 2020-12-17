package adventofcode20201217

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	var version:Int = 0
	
	case class Vect3(x:Int, y:Int, z:Int, w:Int){
		def add(v:Vect3):Vect3 = Vect3(x + v.x, y + v.y, z + v.z, w + v.w)
		
		def key = s"$x $y $z $w"
		
		override def toString() = key
	}
	
	object Vect3{		
		def apply(str:String):Vect3 = {
			val coords = str.split(" ").map(_.toInt)
			Vect3(coords(0), coords(1), coords(2), coords(3))
		}
	}
		
	case class Grid(){
		val g = scala.collection.mutable.Map[String, Boolean]()
		
		def read(lines:List[String]):Unit = {
			for(y <- 0 until lines.length; x <- 0 until lines(0).length) g.update(Vect3(x, y, 0, 0).key, lines(y).charAt(x) == '#')
		}
		
		def neighbours(v:Vect3):List[Vect3] = {
			(for(x <- -1 until 2; y <- -1 until 2; z <- -1 until 2; w <- -1 * version until version + 1
				 if(!((x == 0)&&(y == 0)&&(z == 0)&&(w == 0)))) yield v.add(Vect3(x, y, z, w))).toList
		}
		
		def active(v:Vect3):Boolean = if(g.contains(v.key)) g(v.key) else false
		
		def activeNeighbours(v:Vect3):Int = {
			neighbours(v).count(test => active(test))
		}
		
		def cloneFunc(v: Vect3):Tuple2[String, Boolean] = {
			val ans = activeNeighbours(v)
			
			v.key -> (if(active(v)) ((ans == 2) || (ans == 3)) else (ans == 3))
		}
		
		override def clone():Grid = {
			val newG = Grid()
			
			for((k, v) <- g; n <- neighbours(Vect3(k)) if(!newG.g.contains(n.key))) newG.g += cloneFunc(n)
			
			newG
		}
		
		def allActive:Int = {
			var sum:Int = 0
			for((_, v) <- g) if(v) sum += 1
			sum
		}
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {		
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		version = input._2
		
		var g = Grid()
			
		g.read(lines)
		
		println("initial", g.g.size, g.allActive)
		
		for(i <- 0 until 6){
			g = g.clone()
			
			println("cycle", i, g.g.size, g.allActive)
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
