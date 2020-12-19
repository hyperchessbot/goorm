package adventofcode20201219

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
		
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		//if(input._1 != "example") return
		
		if(lines.length > 0){
			val part = input._2 + 1
			
			val inputParts = lines.mkString("\n").split("\n\n").toList
		
			var rules = inputParts(0).split("\n").map(line => {val parts = line.split(":");(parts(0), parts(1))}).toMap
			
			if(part == 2){
				rules = rules + ("8" -> "42+")
				rules = rules + ("11" -> "42 31 |42 42 31 31 |42 42 42 31 31 31 |42 42 42 42 31 31 31 31 |42 42 42 42 42 31 31 31 31 31 |42 42 42 42 42 42 31 31 31 31 31 31 |42 42 42 42 42 42 42 31 31 31 31 31 31 31 |42 42 42 42 42 42 42 42 31 31 31 31 31 31 31 31 |42 42 42 42 42 42 42 42 42 31 31 31 31 31 31 31 31 31 ")
			}
			
			while(rules.size > 1){
				val k = rules.keySet.find(!rules(_).exists(Character.isDigit(_))).get; val v = rules(k)
				
				rules = (for((k1, v1) <- rules; if k1 != k) yield k1 -> ("\\b"+ k + "\\b").r.replaceAllIn(s"$v1", s"($v)")).toMap
			}
			
			println(inputParts(1).split("\n").count(_.matches(("^" + rules("0").replaceAll("[ \"]", "") + "$"))))
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
