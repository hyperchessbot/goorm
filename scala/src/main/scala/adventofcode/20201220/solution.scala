package adventofcode20201220

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	case class Tile(tileStr:String){
		var lines = tileStr.split("\n")
		
		var id = -1
		
		var repr, top , right, bottom, left:String = ""
		var width, height = 0
		
		var allReprs = List[String]()
		
		def rotate():Unit = {
			repr = repr.substring(width, 4 * width - 1) + repr.substring(0, width)
		}
		
		var flipped = false
		
		def flip():Unit = {
			repr = repr.reverse
			flipped = !flipped
		}
		
		def next():Unit = {
			if(flipped){
				flip()
			}else{
				rotate()
			}
		}
		
		def init():Unit = {		
			lines(0) match {
				case s"Tile $num:" => id = num.toInt
				case _ => {
					println("could not establish tile id")
					return
				}
			}
			
			lines = lines.tail

			width = lines(0).length
			height = lines.length

			if(width != height){
				println("tile is not square")
				return
			}
			
			top = lines(0)
			left = lines.map(_.substring(0, 1)).mkString("").reverse
			right = lines.map(_.substring(width - 1, width)).mkString("")
			bottom = lines(height - 1).reverse
			
			repr = top + right + bottom + left
		}
		
		init()
		
		override def toString():String = s"[Tile $id = $repr]"
	}
	
	var root = 0
	
	var width = 0
	var height = 0
	
	var grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
	
	def arrange(x:Int, y:Int, available:List[Tile]):List[Int] = {
		for(tile <- available){
			
		}
		List()
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		if(input._1 != "example") return
			
		val tiles = lines.mkString("\n").split("\n\n").map(Tile(_)).toList
		
		root = math.sqrt(tiles.length).toInt
		
		width = root
		height = root
		
		width = 2
		height = 1
		
		grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
		
		println(arrange(0, 0, tiles))
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0), ("example2", 1), ("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
