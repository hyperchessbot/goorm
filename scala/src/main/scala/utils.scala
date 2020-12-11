package object utils{	
	def getLinesOf(path:String):List[String] = scala.io.Source.fromFile(path).getLines().toList	
	
	def p(content:String):Unit = println(content)
	
	def time[R](message:String, unit:String, block: => R): R = {
		val t0 = System.nanoTime()
		val result = block    // call-by-name
		val t1 = System.nanoTime()
		unit match {
			case "ns" => println(message + " took " + (t1 - t0) + " ns")
			case "mis" => println(message + " took " + ((t1 - t0)/1e3).round + " mis")
			case "ms" => println(message + " took " + ((t1 - t0)/1e6).round + " ms")
		}		
		result
	}
}
