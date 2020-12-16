package adventofcode20201216

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	case class Range(from:Int, to:Int){
		var okIndices = Set[Int]()
		
		override def toString() = s"[range $from - $to ok $okIndices]"
		
		def ok(value:Int):Boolean = {
			val ok = ( value >= from ) && ( value <= to )			
			ok
		}
	}
	
	case class RangeCollection(name:String, ranges:List[Range]){
		var okIndices = Set[Int]()
		
		def validate(ticket:Ticket, index:Int):Boolean = {
			val value = ticket.values(index)
			
			ticket.errorRate(ranges, List(value)) == 0
		}
		
		override def toString() = s"[range collection $name ok $okIndices]"
	}
	
	case class Ticket(values:List[Int] = List[Int]()){
		override def toString() = s"[ticket $values]"
		
		def errorRate(ranges:List[Range], values:List[Int]):Int = {
			var errorRate = 0
			for(value <- values){
				var ok = false
				for(i <- 0 until ranges.length){
					if(ranges(i).ok(value)) ok = true
				}
				if(!ok) errorRate += value
			}
			errorRate
		}
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		var allRanges = List[Range]()
		var allRangeCollections = List[RangeCollection]()
		var okTickets = List[Ticket]()
		var allErrorRate = 0
		
		var scanNearby = false
		var scanYour = false
		var yourTicket = Ticket()
		lines.foreach(line => {
			line match {
				case s"${name}: ${value}" => {
					val ranges = value.split(" or ").map(_.split("-")).map(item => Range(item(0).toInt, item(1).toInt)).toList
					allRanges = allRanges :++ ranges					
					allRangeCollections = allRangeCollections :+ RangeCollection(name, ranges)
				}
				case s"your ticket${rest}" => {
					scanYour = true
				}
				case s"nearby tickets${rest}" => {
					scanYour = false
					scanNearby = true
				}
				case _ => {
					if(scanYour){
						yourTicket = Ticket(line.split(",").map(_.toInt).toList)
						scanYour = false
					}
					if(scanNearby){
						val ticket = Ticket(line.split(",").map(_.toInt).toList)
						val errorRate = ticket.errorRate(allRanges, ticket.values)
						allErrorRate += errorRate
						if(errorRate == 0) okTickets = okTickets :+ ticket
					}
				}
			}
		})
		
		println(allErrorRate)
		
		var okIn = Map[Int, Set[String]]()
		var failedIn = Map[Int, Set[String]]()
		
		var maxJ = 0
		
		for(ticket <- okTickets){			
			for(j <- 0 until ticket.values.length){
				if(j > maxJ) maxJ = j
				for(i <- 0 until allRangeCollections.length){
					if(allRangeCollections(i).validate(ticket, j)){
						if(okIn.contains(j)){
							val current = okIn(j)
							
							okIn = okIn + (j -> (current + allRangeCollections(i).name))
						}else{
							okIn = okIn + (j -> Set(allRangeCollections(i).name))
						}
					}else{
						if(failedIn.contains(j)){
							val current = failedIn(j)
							
							failedIn = failedIn + (j -> (current + allRangeCollections(i).name))
						}else{
							failedIn = failedIn + (j -> Set(allRangeCollections(i).name))
						}
					}
				}	
			}
		}
		
		var found = Set[String]()
		
		var indices = Map[Int, String]()
		
		for(outer <- 0 until (maxJ + 1)){
			for(j <- 0 until (maxJ + 1)){
				if(!failedIn.contains(j)) failedIn = failedIn + (j -> Set[String]())
				val diff = (okIn(j) -- failedIn(j) -- found)
				if(diff.size == 1){					
					val name = diff.toList(0)					
					if(!indices.contains(j)){
						println("new", outer, j, name)
						indices = indices + (j -> name)
						found = found + name	
					}else{
						println("old", outer, j, name)
					}
				}
			}
		}
		
		val filtered = indices.filter(index => index._2.contains("departure"))
		
		var mult = 1L
		var i = 1
		
		println(filtered)
		
		for(index <- filtered){			
			val value = yourTicket.values(index._1)
			val name = index._2
			mult *= value.toLong
			println(i, name, index._1, value, mult)			
			i += 1
		}
		
		println(mult)
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(
			("example", 0),
			("input", 0),
			//("example2", 1),
			//("input", 1)
		)) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
