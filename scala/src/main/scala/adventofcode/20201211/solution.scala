package adventofcode20201211

import scala.language.postfixOps
import utils._

object problem{
	def solve():Unit = {  
		val prefix = "src/main/scala/adventofcode/20201211/"
		for(input <- List("example") map(prefix + _)){    
			p(s"\nfor '$input' :\n")

			val lines = getLinesOf(s"$input.txt")
			
			p(lines mkString("\n"))
		}
	}
}
