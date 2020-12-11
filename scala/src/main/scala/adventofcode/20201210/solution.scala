package adventofcode20201210

import scala.language.postfixOps
import utils._

object problem{
	def solve():Unit = {  
		val prefix = "src/main/scala/adventofcode/20201211/"
		for(input <- List("example", "example2", "input") map(prefix + _)){    
			p(s"\nfor '$input' :\n")

			val jolts:List[Int] = 0 +: getLinesOf(s"$input.txt").map(_.toInt).toList.sorted

			val diffs = jolts.tail.zipWithIndex.map{case(jolt, i) => jolt - jolts(i)} :+ 3

			p(diffs mkString(" "))

			val ones = diffs count(_ == 1)
			val threes = diffs count(_ == 3)

			p((ones * threes) toString)

			def vars(pocketSize:Int):Long = List(1,1,2,4,7)(pocketSize)

			val pockets = diffs.map(_.toString).mkString.split("3").map(_.length).toList

			p(pockets mkString(" "))
			p(pockets.map(vars(_)).reduce(_ * _).toString)
		}
	}
}
