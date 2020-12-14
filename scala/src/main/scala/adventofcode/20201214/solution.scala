package adventofcode20201214

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	val memSize = 36
	
	def parseLong(str:String):Long = {
		val high = Integer.parseInt(str.substring(0, memSize / 2), 2).toLong
		val low = Integer.parseInt(str.substring(memSize / 2, memSize), 2).toLong
		(high << (memSize / 2)) + low
	}
	
	case class Instruction(instrStr:String){
		val parts = instrStr.split(" = ")
		var command = parts(0)
		val arg = parts(1)
		var andMask:Long = (2L << memSize) - 1
		var orMask:Long = 0
		var address:Long = 0
		var value:Long = 0
		command match {
			case "mask" => {
				andMask = parseLong(arg.replaceAll("X|1", "1"))
				orMask = parseLong(arg.replaceAll("X|0", "0"))
			}
			case _ => {
				address = command.split("\\[|\\]")(1).toLong
				value = arg.toLong				
			}
		}
		println(arg)
		println(andMask.toBinaryString)
		println(orMask.toBinaryString)
	}
	
	case class Machine(instructions:List[Instruction]){
		val memory = scala.collection.mutable.Map[Long, Long]()
		
		var andMask:Long = ( 2L << memSize ) - 1
		var orMask:Long = 0
		
		def execInstr(ins:Instruction):Unit = {
			ins.command match {
				case "mask" => {
					andMask = ins.andMask
					orMask = ins.orMask
				}
				case _ => {
					memory(ins.address) = ins.value & andMask | orMask
				}
			}
		}
		
		instructions.foreach(execInstr(_))
		
		def sum:Long = (for((_, value) <- memory) yield value).sum
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
			
		val instructions = lines.map(Instruction(_))
		
		val machine = Machine(instructions)
		
		println(machine.sum)
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0)/*, ("example", 1), ("input", 1)*/)) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
