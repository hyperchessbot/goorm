package adventofcode20201222

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	case class Game(var playerCards:Array[Array[Int]], version:Int = 0, level:Int = 0){
		var total = 0
		
		var configs = List[Array[Array[Int]]]()

		def printCards(playerCards:Array[Array[Int]]):Unit = {
			println(playerCards(0).toList.mkString(" , "))
			println(playerCards(1).toList.mkString(" , "))
		}
		
		def printConfigs():Unit = {
			for(config <- configs.zipWithIndex){
				println(s"level $level round ${config._2 + 1}")
				printCards(config._1)
				println("------------------------------")
			}
		}
		
		def saveConfig():Unit = {
			configs = configs :+ playerCards.map(player => player.map(card => card))
		}

		def playRound():Int = {
			if(version == 1){
				
			}
			
			val head0 = playerCards(0).head
			val head1 = playerCards(1).head
			
			val winner = if(head0 > head1) 0 else 1
			
			playerCards = playerCards.map(_.tail)
			playerCards(winner) = playerCards(winner) :++ Array(head0, head1).sorted.reverse
			
			total = playerCards(winner).zipWithIndex.map(a => a._1 * ( playerCards(winner).length - a._2 ) ).sum
			
			saveConfig()
			
			if((playerCards(0).length != 0) && (playerCards(1).length != 0)) return -1
			
			return winner
		}
	
		def playGame():Unit = {
			while(playRound() == -1){}
		}
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		if(lines.length > 0){
			
			val game = Game(lines.mkString("\n").split("\n\n").map(_.split("\n").tail.map(_.toInt)), input._2)

			game.playGame()
			
			if(input._1 == "example") game.printConfigs()
			
			println(game.total)
			
		}
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0), ("example", 1), ("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
