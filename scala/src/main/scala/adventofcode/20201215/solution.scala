package adventofcode20201215

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
			
		//lines.foreach(line =>
			val nums = lines.last.split(",").map(_.toInt)
			var spoken = nums.zipWithIndex.map(item => item._1 -> List(item._2)).toMap
			var lastSpoken = nums.last
			
			for(i <- nums.length until (if(input._2 == 0) 2020 else 30000000)){				
				val sl = spoken(lastSpoken)
				
				if(sl.length > 1){
					lastSpoken = sl.last - sl.head
				}else{
					lastSpoken = 0
				}
				
				val nl = if(spoken.contains(lastSpoken)) List(spoken(lastSpoken).last, i) else List(i)
				
				spoken = spoken + ( lastSpoken -> nl )
			}
			
			println(lastSpoken)
		//})
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(/*("example", 0), */("input", 0), /*("example2", 1), */("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
