package adventofcode20201219

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	case class Rule(ruleStr:String){		
		val parts = ruleStr.split(": ").toList
		
		val index = parts(0).toInt
		
		var subrules = List[String]()
		var letter = ""
		
		if(parts(1).contains("\"")){
			val parts2 = parts(1).split("\"")
			letter = parts2(1)
		}else{
			subrules = parts(1).split(" \\| ").toList		
		}
		
		override def toString():String = s"[Rule $index = $letter $subrules]"
		
		def tuple = index -> this
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		//if(input._1 != "example") return
		
		if(lines.length > 0){
			val parts = lines.mkString("\n").split("\n\n").toList
		
			val rules = parts(0).split("\n").map(Rule(_).tuple).toMap
						
			var good = 0
			
			def check(tokens:String):Boolean = {				
				if(tokens == " 0 ") return true
				
				for((key, rule) <- rules){
					if(rule.letter != ""){
						if(tokens.contains(rule.letter)){
							val result = check(tokens.replaceAll(rule.letter, key.toString))
							if(result) return true
						}						
					}else{
						for(subrule <- rule.subrules){
							if(tokens.contains(" " + subrule + " ")){								
								val result = check(tokens.replaceAll(" " + subrule + " ", " " + key.toString + " "))
								if(result) return true
							}							
						}
					}
				}
				
				false
			}
			
			parts(1).split("\n").foreach(msg => {				
				val result = check(" " + msg.split("").mkString(" ") + " ")
				
				if(result) good += 1
				
				println(msg, result, good)
			})
			
			println("good", good)
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
