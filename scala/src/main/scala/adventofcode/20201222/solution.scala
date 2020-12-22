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
		
		def indent = List.fill(level * 2){" "}.mkString

		def printCards():Unit = {
			println(indent + s"level $level round ${configs.length}")
			
			println(indent + playerCards(0).toList.mkString(" , "))
			println(indent + playerCards(1).toList.mkString(" , "))
		}
				
		def saveConfig():Unit = {
			configs = configs :+ playerCards.map(player => player.map(card => card))
		}
		
		var winner = -1

		def playRound():Int = {
			if(version == 1){
				if(configs.exists(config => {
					( config(0).sameElements(playerCards(0)) ) && ( config(1).sameElements(playerCards(1)) )
				})){					
					return 0
				}
			}
			
			saveConfig()
			
			printCards()
			
			val head0 = playerCards(0).head
			val tail0 = playerCards(0).tail
			val head1 = playerCards(1).head
			val tail1 = playerCards(1).tail
			
			winner = if(head0 > head1) 0 else 1
			
			if(version == 1){
				if( ( tail0.length >= head0 ) && ( tail1.length >= head1 ) ){
					val subGame = Game(Array(tail0.slice(0, head0), tail1.slice(0, head1)), version, level + 1)
					
					winner = subGame.playGame()
				}
			}
			
			val winnerHead = playerCards(winner).head
			val loserHead = playerCards(1 - winner).head
			
			playerCards = playerCards.map(_.tail)
			playerCards(winner) = playerCards(winner) :++ Array(winnerHead, loserHead)
			
			total = playerCards(winner).zipWithIndex.map(a => a._1 * ( playerCards(winner).length - a._2 ) ).sum
			
			if((playerCards(0).length != 0) && (playerCards(1).length != 0)) return -1
			
			return winner
		}
	
		def playGame():Int = {
			while(playRound() == -1){}
			
			println(indent + s"********************* final standing ( winner is player ${winner + 1} )")
			printCards()
			println(indent + "*********************")
			
			return winner
		}
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		if(input._1 != "example") return
		
		if(lines.length > 0){
			
			val game = Game(lines.mkString("\n").split("\n\n").map(_.split("\n").tail.map(_.toInt)), input._2)

			game.playGame()
			
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
