package adventofcode20201220

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	val seaMonster = patternToMap(List[String](
		"                  # ",
		"#    ##    ##    ###",
		" #  #  #  #  #  #   "
	))
	
	case class Tile(tileStr:String){
		var lines = tileStr.split("\n").toList
		
		var id = -1
		
		var repr:String = ""
		
		var width, height = 0
		
		var rot = 0
		var flipped = false
		
		def top:String = repr.substring(0, width)
		def right:String = repr.substring(width, 2 * width)
		def bottom:String = repr.substring(2 * width, 3 * width).reverse
		def left:String = repr.substring(3 * width, 4 * width).reverse
		
		def rotate():Unit = {
			repr = repr.substring(3 * width, 4 * width) + repr.substring(0, 3 * width)
			rot += 1
		}
		
		def flip():Unit = {
			repr = repr.reverse
			flipped = !flipped
		}
		
		def next():Unit = {
			rotate()
			if(rot == 4){
				rot = 0
				flip()
			}
		}
		
		def init():Unit = {		
			lines(0) match {
				case s"Tile $num:" => id = num.toInt
			}
			
			lines = lines.tail

			width = lines(0).length
			height = lines.length
			
			repr = 	lines(0) +
					lines.map(_.substring(width - 1, width)).mkString("") +					
					lines(height - 1).reverse +
					lines.map(_.substring(0, 1)).mkString("").reverse
		}
		
		init()
		
		def inner(ty:Int):String = {
			val tlines = transform(lines, flipped, rot)
			
			tlines(ty + 1).substring(1, width - 1)
		}
	}
	
	var root = 0
	
	var gridWidth = 0
	var gridHeight = 0
	var width = 0
	var height = 0
	
	var grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
	
	def arrange(x:Int, y:Int, available:List[Tile]):Boolean = {
		for(tile <- available){			
			if((x==0)&&(y==0)){
				grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
			}
			
			for(perm <- 0 until 8){
				var ok = true
				
				if(grid.contains((x - 1, y))){					
					if(grid((x - 1, y)).right != tile.left) ok = false
				}
				
				if(grid.contains((x, y - 1))){
					if(grid((x, y - 1)).bottom != tile.top) ok = false
				}
				
				if(ok){
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
				
				tile.next()
			}
		}
		
		false
	}
	
	var tiles = List[Tile]()
	
	def searchSeaMonsters():Unit = {
		var buff = ""
		
		for(y <- 0 until gridHeight){			
			for(ty <- 0 until tiles(0).height - 2){				
				for(x <- 0 until gridWidth){					
					buff += grid((x,y)).inner(ty)
				}
			}
		}
		
		val buffWidth = gridWidth * ( tiles(0).width - 2 )
		
		val buffOld = buff
		
		for(perm <- 0 until 8){
			var rot = perm
			var flipped = false
			if(perm > 3){
				rot = perm - 4
				flipped = true
			}
			
			buff = transform(buffOld.grouped(buffWidth).toList, flipped, rot).mkString("")
			
			val matrix = patternToMap(buff.grouped(buffWidth).toList)
			
			var result = hasPattern(matrix, buffWidth, buffWidth, seaMonster)
			
			for((x, y) <- result) removePattern(matrix, seaMonster, x, y)
			
			if(result.length > 0){
				println(matrix.size)
				
				return
			}
		}
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		val test = "..#.\n...#\n.#..\n....".split("\n").toList
				
		if(input._2 == 0) return
			
		tiles = lines.mkString("\n").split("\n\n").map(Tile(_)).toList
		
		root = math.sqrt(tiles.length).toInt
		
		gridWidth = root
		gridHeight = root
				
		width = tiles(0).width
		height = tiles(0).height
		
		grid = scala.collection.mutable.Map[Tuple2[Int, Int], Tile]()
		
		val result = arrange(0, 0, tiles)
		
		if(result){
			val topLeft = grid(0,0).id.toLong
			val topRight = grid(gridWidth-1, 0).id.toLong
			val bottomLeft = grid(0,gridHeight-1).id.toLong
			val bottomRight = grid(gridHeight-1,gridWidth-1).id.toLong

			val check = topLeft * topRight * bottomLeft * bottomRight

			println(check)	
			
			searchSeaMonsters()
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
