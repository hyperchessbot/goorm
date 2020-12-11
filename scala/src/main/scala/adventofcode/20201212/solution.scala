package adventofcode20201212

import scala.language.postfixOps
import utils._
import table._

object problem{
	def solve():Unit = {
		val prefix = "src/main/scala/adventofcode/20201212/"
		
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0), ("example", 1), ("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n${input._1} version ${input._2} processing")
			
			val table = Table(getLinesOf(s"$prefix${input._1}.txt"))
			
			def getVisibleOccup(table:Table, x:Int, y:Int):Int = {
				var sum = 0
				for(dx <- List(-1, 0, 1); dy <- List(-1, 0, 1)){
					var ok = true
					var found = false
					var i = 1					
					while(table.okXYd(x, y, i * dx, i * dy) && ok && (!found)){
						val current = table.getXY(x + i * dx, y + i * dy)
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
			
			def getNumOccup(table:Table, x:Int, y:Int, v:Int):Int = {
				table.countInNeighbours(x, y, "#") + v * getVisibleOccup(table, x, y)
			}

			def evolve(table: Table, v:Int):Table = {
				val newTable = table.clone()			

				for(y <- 0 until table.height; x <- 0 until table.width){
					if(table.getXY(x, y) != "."){					
						if(getNumOccup(table, x, y, input._2) == 0) newTable.setXY(x, y, "#")
						else if(getNumOccup(table, x, y, input._2) >= (4 + input._2 * v)) newTable.setXY(x, y, "L")
					}
				}

				newTable
			}
			
			//println(table)
			
			var currentTable = table
			var newTable = table
			
			var i = 0
			
			do{
				currentTable = newTable
				//println("iteration", i)
				newTable = evolve(currentTable, input._2)
				//println(newTable)
				i += 1
			}while(!newTable.equal(currentTable) && i < 1000)
			
			println(s"iterations ${i}, occupied ${newTable.countInAll("#")}")
		})
		
		println(delim)
	}
}
