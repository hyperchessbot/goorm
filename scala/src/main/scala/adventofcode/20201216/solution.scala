package adventofcode20201216

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
			
				
		
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(
			("example", 0),
			("input", 0),
			("example2", 1),
			("input", 1)
		)) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
