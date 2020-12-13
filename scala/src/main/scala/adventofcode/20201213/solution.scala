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
	
	def solve2brute(lines:List[String]):Unit = {
		val reqBuses = lines(1).split(",").map(str => if(str == "x") 0 else str.toLong).toList
		val buses = reqBuses.filter(_ != 0).toList
		
		var i = 0L
		var ok = false
		
		while((i < 2000000)&&(!ok)){
			ok = true						
			var found = Set[Int]()
			for(j <- 0 until reqBuses.length if ok){								
				for(bus <- buses if ok){
					val hasBus = (i + j) % bus == 0
					if(reqBuses(j) != 0) if(hasBus && (reqBuses(j) == bus)) found = found + j
					if(hasBus && (reqBuses(j) == 0)) ok = false
				}
			}			
			if(found.size < buses.length) ok = false			
			if(!ok) i += 1			
		}		
		println(i)
	}
	
	def solve2(lines:List[String]):Unit = {
		val reqBuses = lines(1).split(",").zipWithIndex.filter(_._1 != "x").map(bus => bus._2 -> bus._1.toLong).toList
		
		var jump = reqBuses(0)._2
		
		var timeStamp = 0L
		
		for((index, bus) <- reqBuses.tail){
			while((timeStamp + index) % bus != 0){
				timeStamp += jump	
			}            
        	jump *= bus
		}
		
		println(timeStamp)
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
