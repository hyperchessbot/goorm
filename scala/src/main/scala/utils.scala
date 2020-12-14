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
	
	case class IntVect(x:Int, y:Int){
		def add(v:IntVect):IntVect = {
			IntVect(x + v.x, y + v.y)
		}
		
		def mult(s:Int):IntVect = {
			IntVect(x * s, y * s)
		}
		
		def rot():IntVect = {
			IntVect(y, -x)
		}
		
		def turn(d:Int):IntVect = {			
			var times = (d / 90).abs % 4			
			if(d < 0) times = (4 - times) % 4
			var current = IntVect(x, y)			
			for(i <- 0 until times) current = current.rot()
			current
		}
		
		override def toString():String = s"Vect($x, $y)"
		
		def manhattan:Int = x.abs + y.abs
	}
	
	val parseBinaryToLongChunkSize = 16
	
	def parseBinaryToLong(binary:String):Long = {
		var buffer = 0L
		var current = binary
		
		do{
			buffer *= ( 1L << Math.min(parseBinaryToLongChunkSize, current.length) )
			
			if(current.length <= parseBinaryToLongChunkSize){
				buffer += Integer.parseInt(current, 2)
				
				return buffer
			}

			buffer += Integer.parseInt(current.substring(0, parseBinaryToLongChunkSize), 2)

			current = current.substring(parseBinaryToLongChunkSize)
		}while(current.length > 0)
		
		buffer
	}
}
