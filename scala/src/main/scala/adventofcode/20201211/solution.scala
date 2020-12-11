package adventofcode20201211

import scala.language.postfixOps
import utils._

object problem{
	def solve():Unit = {  
		val prefix = "src/main/scala/adventofcode/20201211/"
		for(input <- List(("example", 0), ("input", 0), ("example", 1), ("input", 1))){    
			println (input)

			var lines:Array[Array[String]] = getLinesOf(s"$prefix${input._1}.txt").toArray.map(_.split("").toArray)
			
			val height = lines.length
			val width = lines(0).length
			
			def neighbourOk(x: Int, y: Int, dx: Int, dy: Int):Boolean = {								
				if((dx == 0) && (dy==0)) return false				
				if(x + dx < 0) return false
				if(x + dx >= width) return false
				if(y + dy < 0) return false
				if(y + dy >= height) return false
				true
			}
			
			def getNeighbours(lines:Array[Array[String]], x:Int, y:Int):List[String] = (for(dx <- List(-1, 0, 1); dy <- List(-1, 0, 1) if neighbourOk(x, y, dx, dy)) yield lines(y + dy)(x + dx)).toList
			
			def getVisibleOccup(lines:Array[Array[String]], x:Int, y:Int):Int = {
				var sum = 0
				for(dx <- List(-1, 0, 1); dy <- List(-1, 0, 1)) if(!((dx == 0) && (dy == 0))){
					var ok = true
					var found = false
					var i = 1					
					while(neighbourOk(x, y, i * dx, i * dy) && ok && (!found)){
						val current = lines(y + i * dy)(x + i * dx)
						if(current == "#"){
							if(i > 1) found = true
							ok = false
						} else if(current != ".") ok = false
						i += 1
					}
					if(found) sum += 1
				}
				sum
			}
			
			def getNumOccup(lines:Array[Array[String]], x:Int, y:Int, v:Int):Int = getNeighbours(lines, x, y).count(_ == "#") + v * getVisibleOccup(lines, x, y)
			
			def getAllOccup(lines:Array[Array[String]]):Int = lines.flatten.count(_ == "#")
			
			def evolve(lines: Array[Array[String]], v:Int):Array[Array[String]] = {
				val newLines:Array[Array[String]] = (0 until height).map(y => (0 until width).map(x => lines(y)(x)).toArray).toArray
				
				for(y <- 0 until height; x <- 0 until width){
					if(lines(y)(x) != "."){
						newLines(y)(x) = lines(y)(x)
						if(getNumOccup(lines, x, y, input._2) == 0) newLines(y)(x) = "#"
						else if(getNumOccup(lines, x, y, input._2) >= (4 + input._2)) newLines(y)(x) = "L"
					}
				}
				
				newLines
			}
			
			def prettyPrint(lines:Array[Array[String]]):Unit = {				
				for(line <- lines) println(line.mkString(""))
				println("-------------")
			}
			
			def equal(lines: Array[Array[String]], newLines: Array[Array[String]]):Boolean = lines.flatten.mkString == newLines.flatten.mkString
			
			var newLines = lines
			var i = 0
			
			do{
				lines = newLines
				//println("iteration", i)
				newLines = evolve(lines, input._2)
				//prettyPrint(newLines)
				i += 1
			}while(!equal(newLines, lines) && i < 1000)
			
			println(getAllOccup(lines))
		}
	}
}
