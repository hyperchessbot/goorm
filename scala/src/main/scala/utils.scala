package object utils{	
	// max coord display length for vector
	val maxCoordLength = 10
	
	// trim a string to max number of characters
	def ellipsis(str:String, max: Int):String = {
		val l = str.length
		if(l <= max) return str
		return str.substring(0, max - 2) + ".."
	}

	// base class of vect
	abstract class Vect[T](setCoords: Seq[T]){
		val coords = scala.collection.mutable.ArrayBuffer[T](setCoords: _*)
		def add(a:T, b:T):T
		def mult(a:T, b:T):T
		def zero:T
		def newVect(setCoords: Seq[T]):Vect[T]  
		override def toString():String = s"Vect[${coords.map(str => ellipsis(str.toString, maxCoordLength)).mkString(",")}]"
		def createSynced(v:Vect[T]):Vect[T] = {
			val temp = newVect(coords.toSeq)
			while(temp.coords.length < v.coords.length) temp.coords += zero    
			temp
	  	}
		
		def +(v:Vect[T]):Vect[T] = {
			val temp = createSynced(v)
			for(i <- 0 until v.coords.length) temp.coords(i) = add(temp.coords(i), v.coords(i))
			temp
		}
  
		def *(s:T):Vect[T] = {
			val temp = createSynced(this)
			for(i <- 0 until temp.coords.length) temp.coords(i) = mult(temp.coords(i), s)
			temp
		}
	}

	// dervied Int vect
	class IntegerVect(setCoords: Seq[Int]) extends Vect[Int](setCoords: Seq[Int]){
		def add(a: Int, b:Int) = a + b
		def mult(a: Int, b: Int) = a * b
		def zero:Int = 0
		def newVect(setCoords: Seq[Int]) = new IntegerVect(setCoords)
	}

	// derived Double vect
	class DoubleVect(setCoords: Seq[Double]) extends Vect[Double](setCoords: Seq[Double]){
		def add(a: Double, b:Double) = a + b
		def mult(a: Double, b: Double) = a * b
		def zero:Double = 0.0
		def newVect(setCoords: Seq[Double]) = new DoubleVect(setCoords)
	}

	object Vect{
		def apply(setCoords: Int*):IntegerVect = new IntegerVect(setCoords)
		def apply(setCoords: Double*):DoubleVect = new DoubleVect(setCoords)
	}
	
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
