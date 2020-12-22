package adventofcode20201222

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	case class Player(id:Int, initialCards: Seq[Int]){
		var cards = scala.collection.mutable.ArrayBuffer[Int](initialCards:_*)
		override def clone():Player = Player(id, cards.clone.toSeq)
		def total = cards.zipWithIndex.map(item => (cards.length - item._2) * item._1).sum
		override def toString():String = s"[Player $id , cards = ${cards.mkString(" , ")}]"
		def draw:Int = cards.remove(0)
	}

	case class Game(players:Seq[Player], part:Int = 1){
		val (player1, player2 ) = (players(0).clone, players(1).clone)
		val configs = scala.collection.mutable.Set[String]()
		def config = player1.toString() + " | " + player2.toString()
		def result = if(configs.contains(config)) 1 else if(player1.cards.length == 0) 2 else if(player2.cards.length == 0) 1 else 0
		def total = if(result == 0) 0 else if(result == 1) player1.total else player2.total    
		def playRound():Boolean = {      
		  if(result != 0) return false      
		  configs += config
		  val card1 = player1.draw
		  val card2 = player2.draw
		  var winner = if(card1 > card2) 1 else if(card2 > card1) 2 else 0      
		  if(part == 2) if((card1 <= player1.cards.length) && (card2 <= player2.cards.length)) winner = Game(Seq(Player(1, player1.cards.slice(0, card1).toSeq), Player(2, player2.cards.slice(0, card2).toSeq))).playGame()
		  if(winner == 1) player1.cards ++= List(card1, card2)
		  if(winner == 2) player2.cards ++= List(card2, card1)
		  true
		}
		def playGame():Int = { while(playRound()){} ; result } ; playGame()
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		//val lines = getLinesOf(s"$prefix${input._1}.txt")
			
		val players = scala.io.Source.fromFile(s"$prefix${input._1}.txt").mkString.split("\n\n").zipWithIndex.map(cards => (Player(cards._2 + 1, cards._1.split("\n").tail.map(_.toInt).toSeq))).toList

    	println(Game(players, input._2 + 1).total)
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
