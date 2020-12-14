package adventofcode20201214

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	val memSize = 36
	
	case class Instruction(instrStr:String){
		val parts = instrStr.split(" = ")
		var command = parts(0)
		val arg = parts(1)
		var andMask:Long = (2L << memSize) - 1
		var orMask:Long = 0
		var floatMask:Long = 0
		var floatDisableMask:Long = (2L << memSize) - 1
		var address:Long = 0
		var value:Long = 0
		command match {
			case "mask" => {
				andMask = parseBinaryToLong(arg.replaceAll("X|1", "1"))
				orMask = parseBinaryToLong(arg.replaceAll("X|0", "0"))
				floatMask = parseBinaryToLong(arg.replaceAll("1|0", "0").replaceAll("X", "1"))				
				floatDisableMask = parseBinaryToLong(arg.replaceAll("1|0", "1").replaceAll("X", "0"))				
			}
			case _ => {
				address = command.split("\\[|\\]")(1).toLong
				value = arg.toLong				
			}
		}
	}
	
	case class Machine(instructions:List[Instruction], v:Int){
		val memory = scala.collection.mutable.Map[Long, Long]()
		
		var andMask:Long = ( 2L << memSize ) - 1
		var orMask:Long = 0
		var floatMask:Long = 0
		var floatDisableMask:Long = 0
		
		def execInstr(ins:Instruction):Unit = {
			v match {
				case 0 => ins.command match {
					case "mask" => {
						andMask = ins.andMask
						orMask = ins.orMask						
					}
					case _ => {
						memory(ins.address) = ins.value & andMask | orMask
					}
				}
				case _ => ins.command match {
					case "mask" => {
						floatMask = ins.floatMask						
						floatDisableMask = ins.floatDisableMask
						orMask = ins.orMask
					}
					case _ => {
						val memMap = scala.collection.mutable.Map[Int, Long]()
						var index = 0
						for(i <- 0 until memSize){
							val mask = 1L << i
							if((floatMask & mask) != 0){
								memMap(index) = mask
								index += 1
							}
						}											
						for(i <- 0 until 2 << memMap.size){							
							var address = ins.address & floatDisableMask | orMask
							for(index <- 0 until memMap.size){								
								if((i & (2 << index)) != 0) address = address | memMap(index)
							}														
							memory(address) = ins.value
						}
					}
				}
			}
			
		}
		
		instructions.foreach(execInstr(_))
		
		def sum:Long = (for((_, value) <- memory) yield value).sum
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
			
		val instructions = lines.map(Instruction(_))
		
		val machine = Machine(instructions, input._2)
		
		println(machine.sum)
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
