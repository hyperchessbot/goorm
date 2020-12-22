package adventofcode20201222

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	case class Game(var playerCards:Array[Array[Int]], version:Int = 0, level:Int = 0, var verbose:Boolean = false){
		var configs = scala.collection.mutable.Set[String]()
		
		def indent = List.fill(level * 2){" "}.mkString

		def printCards():Unit = {			
			println(indent + s"level $level round ${configs.size} , player 1 has ${playerCards(0).length} card(s), player 2 has ${playerCards(1).length} card(s)")
			
			println(indent + playerCards(0).toList.mkString(" , "))
			println(indent + playerCards(1).toList.mkString(" , "))
		}
		
		def printFinalStanding():Unit = {
			println(indent + s"********************* final standing ( winner is player ${winner + 1} )")
			printCards()
			println(indent + "*********************")	
		}
		
		var winner = -1
		
		def config = playerCards(0).mkString(",") + "|" + playerCards(1).mkString(",")

		def playRound():Int = {
			if(version == 1){				
				if(configs.contains(config)) return 0
			}
			
			configs += config
			
			if(verbose) printCards()
			
			val head0 = playerCards(0).head
			val tail0 = playerCards(0).tail
			
			val head1 = playerCards(1).head
			val tail1 = playerCards(1).tail
			
			winner = -1
			
			if(head0 > head1) winner = 0
			if(head1 > head0) winner = 1
			
			if(version == 1){
				if( ( tail0.length >= head0 ) && ( tail1.length >= head1 ) ){
					val subGame = Game(Array(tail0.slice(0, head0), tail1.slice(0, head1)), version, level + 1, verbose = verbose)
					
					winner = subGame.playGame()
				}
			}
			
			if(winner == -1) return 0
			
			val winnerHead = playerCards(winner).head
			val loserHead = playerCards(1 - winner).head

			playerCards = playerCards.map(_.tail)
			
			playerCards(winner) = playerCards(winner) :++ Array(winnerHead, loserHead)	
			
			if((playerCards(0).length != 0) && (playerCards(1).length != 0)) return -1
			
			return winner
		}
		
		def total = playerCards(winner).zipWithIndex.map(a => a._1 * ( playerCards(winner).length - a._2 ) ).sum
	
		def playGame():Int = {
			while(playRound() == -1){}
			
			return winner
		}
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
		
		if(lines.length > 0){
			
			val game = Game(lines.mkString("\n").split("\n\n").map(_.split("\n").tail.map(_.toInt)), input._2, verbose = input._1 == "example")

			game.playGame()
			
			game.printFinalStanding()
			
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
