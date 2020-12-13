package adventofcode20201214

import scala.language.postfixOps
import utils._
import table._

object problem{
	val prefix = "src/main/scala/adventofcode/20201214/"
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
	
		
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
