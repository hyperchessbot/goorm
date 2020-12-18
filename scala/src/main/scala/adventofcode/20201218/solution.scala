package adventofcode20201218

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	def doOp(current:Long, term:Long, op:String):Long = {		
		op match {
			case "+" => return (current + term)
			case "*" => return (current * term)
			case _ => return(term)
		}
		current
	}
	
	def eval(setTerms:List[String], setOp:String, setCurrent:Long, level:Int):Tuple2[Long, List[String]] = {
		var term = setTerms.head
		var rest = setTerms.tail
		var current = setCurrent
		var op = setOp
		
		do{
			term match {
				case "(" => {
					val result = eval(rest, "", current, level + 1)
					rest = result._2
					current = doOp(current, result._1, op)
				}
				case ")" => return (current, rest)
				case "+" => op = "+"
				case "*" => op = "*"
				case _ => current = doOp(current, term.toLong, op)				
			}	
						
			if(rest.length == 0) return (current, List())
			
			term = rest.head
			rest = rest.tail
		}while(true)		
		
		(current, List())
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		var sum = 0L
		
		lines.foreach(line => {
			val terms = line.replaceAll("\\)", " )").replaceAll("\\(", "( ").split(" ").toList
			
			val value = eval(terms, "", 0, 0)._1
			
			sum += value
			
			println(s"$line = $value ( acc = $sum )")
		})
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
