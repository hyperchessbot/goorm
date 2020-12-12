package adventofcode20201212

import scala.language.postfixOps
import utils._
import table._

object problem{
	val prefix = "src/main/scala/adventofcode/20201212/"
	
	case class Instruction(val instrStr:String){
		val command = instrStr.substring(0, 1)
		val arg = instrStr.substring(1).toInt
		override def toString():String = s"$command $arg"
	}
	
	case class Ship(instructions:List[Instruction], var waypoint:Option[IntVect]){
		var pos = IntVect(0, 0)
		var dir = IntVect(1, 0)		
		
		def execInstr(ins:Instruction):Unit = {
			waypoint match {
				case None => ins.command match {
					case "N" => pos = pos.add(IntVect(0, -ins.arg))
					case "S" => pos = pos.add(IntVect(0, ins.arg))
					case "W" => pos = pos.add(IntVect(-ins.arg, 0))
					case "E" => pos = pos.add(IntVect(ins.arg, 0))				
					case "F" => pos = pos.add(dir.mult(ins.arg))
					case "L" => dir = dir.turn(ins.arg)
					case "R" => dir = dir.turn(-ins.arg)
					case _ => println("unknown instruction")
				}
				case _ => ins.command match {
					case "N" => waypoint = Some(waypoint.get.add(IntVect(0, -ins.arg)))
					case "S" => waypoint = Some(waypoint.get.add(IntVect(0, ins.arg)))
					case "W" => waypoint = Some(waypoint.get.add(IntVect(-ins.arg, 0)))
					case "E" => waypoint = Some(waypoint.get.add(IntVect(ins.arg, 0)))
					case "F" => pos = pos.add(waypoint.get.mult(ins.arg))
					case "L" => waypoint = Some(waypoint.get.turn(ins.arg))
					case "R" => waypoint = Some(waypoint.get.turn(-ins.arg))
					case _ => println("unknown instruction")
				}
			}
			
			//println(ins, pos, waypoint, dir)
		}
		
		def execAll():Unit = {
			instructions.foreach(execInstr(_))
		}
		
		override def toString():String = {
			s"ship at pos $pos waypoint ${waypoint} dist ${pos.manhattan}"
		}
		
		execAll()
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val instructions = getLinesOf(s"$prefix${input._1}.txt").map(Instruction(_))
		
		val ship = Ship(instructions, if(input._2 == 0) None else Some(IntVect(10, -1)))
		
		println(ship)
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
