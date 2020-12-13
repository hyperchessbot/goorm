package adventofcode20201213

import scala.language.postfixOps
import utils._
import table._

object problem{
	val prefix = "src/main/scala/adventofcode/20201213/"
	
	def solve1(lines:List[String]):Unit = {
		val timestamp = lines(0).toLong
		val buses = lines(1).split(",").filter(_ != "x").map(_.toLong).toList
		
		//println(timestamp, buses)
		
		var diff = 1e10.toLong
		
		var bestBus = 0L
		
		for(bus <- buses){
			val next = ((timestamp / bus) + 1) * bus
			val currentDiff = next - timestamp
			if(currentDiff < diff){
				diff = currentDiff
				bestBus = bus
			}
			//println(bus, next, currentDiff, bestBus, diff)
		}
		println(bestBus * diff)
	}
	
	def solve2(lines:List[String]):Unit = {
		println("solve2")
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		if(input._2 == 0) solve1(lines) else solve2(lines)
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0), ("example", 1), ("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
