package adventofcode20201211

object problem{
	def solve():Unit = {  
		val prefix = "src/main/scala/adventofcode/20201211/"
	  for(input <- List("example", "example2", "input").map(prefix + _)){    
		println(s"\nfor '$input' :\n")

		val jolts:List[Int] = 0 +: scala.io.Source.fromFile(s"$input.txt").getLines.map(_.toInt).toList.sorted

		val diffs = jolts.tail.zipWithIndex.map{case(jolt, i) => jolt - jolts(i)} :+ 3

		println(diffs)

		val ones = diffs.count(_ == 1)
		val threes = diffs.count(_ == 3)

		println(ones, threes, ones * threes)

		def vars(pocketSize:Int):Long = {
		  val trib = List(1,1,2,4,7)

		  return trib(pocketSize)
		}

		val pockets = diffs.map(_.toString).mkString.split("3").map(_.length).toList

		println(pockets)
		println(pockets.map(vars(_)).reduce(_ * _))
	  }
	}
}
