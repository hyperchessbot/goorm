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
				rules = rules + ("11" -> (for(i <- 1 until 10) yield List.fill(i){"42 "}.mkString("") + List.fill(i){"31 "}.mkString("")).toList.mkString("|"))
			}
			
			while(rules.size > 1){
				val k = rules.keySet.find(!rules(_).exists(Character.isDigit(_))).get
				val v = rules(k)
				
				rules = (for((k1, v1) <- rules; if k1 != k) yield k1 -> s"\\b$k\\b".r.replaceAllIn(v1, s"($v)")).toMap
			}
			
			val pattern = ("^" + rules("0").replaceAll("[ \"]", "") + "$")
			
			writeStringToFile(s"$prefix${input._1}_out_part$part", pattern)
			
			println(inputParts(1).split("\n").count(_.matches(pattern)))
			
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
