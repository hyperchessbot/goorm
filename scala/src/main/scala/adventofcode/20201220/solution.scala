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
		
		var repr:String = ""
		var width, height = 0
		
		var allReprs = List[String]()
		
		var rot = 0
		var flipped = false
		
		def rotate():Unit = {
			repr = repr.substring(3 * width, 4 * width) + repr.substring(0, 3 * width)
			rot += 1
		}
		
		def flip():Unit = {
			repr = repr.reverse
			flipped = !flipped
		}
		
		def next():Unit = {
			if(rot == 4){
				rot = 0
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
			
			repr = 	lines(0) +
					lines.map(_.substring(width - 1, width)).mkString("") +					
					lines(height - 1).reverse +
					lines.map(_.substring(0, 1)).mkString("").reverse
		}
		
		init()
		
		def top():String = repr.substring(0, width)
		def right():String = repr.substring(width, 2 * width)
		def bottom():String = repr.substring(2 * width, 3 * width).reverse
		def left():String = repr.substring(3 * width, 4 * width).reverse
		
		override def toString():String = s"[Tile $id rot $rot flipped $flipped = ${repr.substring(0, width)} | ${repr.substring(width, 2 * width)} | ${repr.substring(2 * width, 3 * width)} | ${repr.substring(3 * width, 4 * width)} ]"
	}
	
	var root = 0
	
	var gridWidth = 0
	var gridHeight = 0
	var width = 0
	var height = 0
	
	var grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
	
	def arrange(x:Int, y:Int, available:List[Tile]):Boolean = {
		for(tile <- available){
			//println("arranging", x, y, available.map(_.id).toList)
			if((x==0)&&(y==0)){
				//println("clearing grid")
				grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
			}
			for(perm <- 0 until 8){
				//println("trying", tile)
				
				tile.next()
				
				var ok = true
				// check left
				if(grid.contains((x - 1, y))){					
					if(grid((x - 1, y)).right() != tile.left()) ok = false
				}
				// check top
				if(grid.contains((x, y - 1))){
					if(grid((x, y - 1)).bottom() != tile.top()) ok = false
				}
				// check bottom
				if(grid.contains((x, y + 1))){
					if(grid((x, y + 1)).top() != tile.bottom()) ok = false
				}
				
				if(ok){
					//println("perm ok")
					
					grid.update((x,y), tile)	
					
					var nextX = x + 1
					var nextY = y
					
					if(nextX >= gridWidth){
						nextX = 0
						nextY = y + 1
					}
					
					if(nextY >= gridHeight) return true
					
					val result = arrange(nextX, nextY, available.filter(_ != tile))
					
					if(result) return true
				}				
			}
		}
		
		false
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		if(input._1 == "example2") return
			
		val tiles = lines.mkString("\n").split("\n\n").map(Tile(_)).toList
		
		root = math.sqrt(tiles.length).toInt
		
		println("num tiles", tiles.length, "root", root)
		
		gridWidth = root
		gridHeight = root
				
		width = tiles(0).width
		height = tiles(0).height
		
		grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
		
		/*val tile = tiles(0)
		
		for(i <- 0 until 8){
			println(tile)
			println(tile.top())
			println(tile.right())		
			println(tile.bottom())
			println(tile.left())
			tile.next()
		}
		println(tile)*/
		
		val result = arrange(0, 0, tiles)
		
		println(grid.keySet)
		
		if(result){
			println("success")
			
			val topLeft = grid(0,0).id.toLong
			val topRight = grid(gridWidth-1, 0).id.toLong
			val bottomLeft = grid(0,gridHeight-1).id.toLong
			val bottomRight = grid(gridHeight-1,gridWidth-1).id.toLong

			println(topLeft, topRight, bottomLeft, bottomRight)

			val check = topLeft * topRight * bottomLeft * bottomRight

			println(check)	
		}else{
			println("failed")
		}
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
