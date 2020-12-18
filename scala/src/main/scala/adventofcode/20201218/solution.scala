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
	
	def precedence(setTerms:List[String]):Tuple2[List[String], List[String]] = {
		var term = setTerms.head
		var rest = setTerms.tail
		
		var acc = List[String]()
		
		var open = false
		
		do{			
			
			term match {
				case "(" => {
					val result = precedence(rest)					
					acc = acc :+ "(" :++ result._1					
					if(open) acc = acc :+ ")"
					open = false
					rest = result._2
				}
				
				case ")" => {										
					return (acc :+ ")", rest)				
				}
				
				case "+" => {
					open = true
					
					var level = 0
					var temp = List[String]()
					
					var ok = true
					
					do{
						if(acc.length == 0){
							ok = false
							acc = "(" +: temp							
						}else{
							val last = acc.last
							
							last match {
								case ")" => {
									level += 1
									acc = acc.init
									temp = last +: temp
								}
								
								case "(" => {
									level -= 1
									acc = acc.init
									temp = last +: temp
									if(level == 0){
										acc = acc :+ "(" :++ temp
										ok = false
									}
								}
								
								case "+" | "-" => {
									acc = acc.init
									temp = last +: temp
								}
								
								case _ => {
									acc = acc.init
									temp = last +: temp
									if(level == 0){
										acc = acc :+ "("
										acc = acc :++ temp
										ok = false
									}
								}
							}
						}
					}while(ok)
					
					acc = acc :+ "+"
				}
				
				case _ => {					
					acc = acc :+ term
					if(open) acc = acc :+ ")"
					open = false
				}
			}
			
			if(rest.length == 0) return (acc, List())
			
			term = rest.head
			rest = rest.tail
		}while(true)
		
		(List(), List())
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		var sum1 = 0.0
		var sum2 = 0.0
		
		lines.foreach(line => {			
			println(line)
			println(ExpressionParser.groupByPrecedenceAsString(line))
			
			sum1 += ExpressionParser.evaluate(line, false)			
			sum2 += ExpressionParser.evaluate(line)			
		})
		
		println(sum1.toLong)
		println(sum2.toLong)
		
		return
		
		var sum = 0L
		
		if(input._2 == 0) return
		
		lines.foreach(line => {
			var terms = line.replaceAll("\\)", " )").replaceAll("\\(", "( ").split(" ").toList
			
			if(input._2 == 1){
				val result = precedence(terms)
				terms = result._1
				println(terms)
			}
			
			val value = eval(terms, "", 0, 0)._1
			
			sum += value
			
			println(s"$line = $value ( acc = $sum )")
		})
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("input", 0))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
