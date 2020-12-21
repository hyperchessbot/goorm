import java.io.PrintWriter

package object utils{	
	def transform(setLines:List[String], flipped:Boolean, setRot:Int):List[String] = {
		var rot = setRot
		var lines = setLines
		
		val width = lines(0).length
		val height = lines.length
		
		if(width != height){
			println("transform width not equal to height", width, height)			
		}else{
			var buff = lines.mkString("")
			
			if(flipped){				
				lines = buff.grouped(width).toList

				buff = ""	

				for(y <- 0 until height){
					for(x <- 0 until width){
						buff += lines(x).substring(y, y + 1)
					}					
				}
			}
			
			if(rot > 0){
				for(r <- 0 until rot){
					lines = buff.grouped(width).toList
			
					buff = ""	
					
					for(y <- 0 until height){
						for(x <- 0 until width){
							buff += lines(height - 1 - x).substring(y, y + 1)
						}					
					}
				}	
			}
						
			return buff.grouped(width).toList
		}
		
		lines
	}
	
	def patternToMap(pattern:List[String], symbol:String = "#"):scala.collection.mutable.Set[Tuple2[Int, Int]] = {
		val out = scala.collection.mutable.Set[Tuple2[Int, Int]]()
		
		for((line, y) <- pattern.zipWithIndex){
			for(x <- 0 until line.length){
				if(line.substring(x, x + 1) == symbol){
					out += Tuple2[Int,Int](x, y)
				}
			}		
		}
		
		out
	}
	
	def hasPattern(matrix:scala.collection.mutable.Set[Tuple2[Int, Int]], width:Int, height:Int, pattern:scala.collection.mutable.Set[Tuple2[Int, Int]]):List[Tuple2[Int, Int]] = {
		var l = List[Tuple2[Int, Int]]()
		
		for(x <- 0 until width; y <- 0 until height){
			var ok = true
			
			for((testX, testY) <- pattern){
				if(!matrix.contains((x + testX, y + testY))) ok = false			
			}
			
			if(ok){
				l = l :+ Tuple2[Int, Int](x, y)
			}
		}
		
		l
	}
	
	def removePattern(matrix:scala.collection.mutable.Set[Tuple2[Int, Int]], pattern:scala.collection.mutable.Set[Tuple2[Int, Int]], x:Int, y:Int) = {
		for((testX, testY) <- pattern){
			matrix -= Tuple2[Int, Int](x + testX, y + testY)
		}
	}
	
	def writeStringToFile(path:String, content:String):Unit = {
		new PrintWriter(path) { write(content); close }
	}
	
	object ExpressionParser{
		def toRawTerms(expressionStr:String):List[String] = {
			expressionStr.replaceAll("\\)", " )").replaceAll("\\(", "( ").split(" ").filter(_ != " ").toList
		}
		
		sealed trait Term{
			def execute(current:Double, value:Double):Double = {
				value
			}
		}
		
		case object EmptyTerm extends Term
		
		sealed trait GroupingTerm extends Term
		
		case object OpeningParenthesis extends GroupingTerm{
			override def toString():String = "("
		}
		
		case object ClosingParenthesis extends GroupingTerm{
			override def toString():String = ")"
		}
		
		case class Operator(kind:String) extends Term{
			override def execute(current:Double, value:Double):Double = {
				kind match {
					case "+" => current + value
					case "*" => current * value
					case _ => current
				}
			}
			
			override def toString():String = kind
		}
		
		sealed trait ValueTerm extends Term
		
		case class IntegerLiteral(val value:Int) extends ValueTerm{
			override def toString():String = value.toString
		}
		
		object Term{
			def apply(termStr:String):Term = {
				termStr match {
					case "(" => OpeningParenthesis
					case ")" => ClosingParenthesis
					case "+" => Operator("+")
					case "*" => Operator("*")
					case _ => {
						IntegerLiteral(termStr.toInt)
					}
				}
			}
		}
		
		def toTerms(termStr:String):List[Term] = {
			toRawTerms(termStr).map(Term(_))
		}
		
		def termsToString(terms:List[Term]):String = terms.map(_.toString).mkString(" ")
		
		def groupByPrecedence(precedenceOperator:Operator, terms:List[Term]):Tuple2[List[Term], List[Term]] = {
			var acc = List[Term]()
			var open = false
			
			if(terms.length == 0) return (acc, acc)
			
			var head = terms.head
			var tail = terms.tail
			
			var lastWholeTermIndex = 0
			
			do{
				head match {
					case OpeningParenthesis => {						
						val result = groupByPrecedence(precedenceOperator, tail)
						
						lastWholeTermIndex = acc.length						
						acc = acc :+ OpeningParenthesis :++ result._1
						
						if(open){
							acc = acc :+ ClosingParenthesis
							open = false
						}
						
						tail = result._2
					}
					
					case ClosingParenthesis => {
						return (acc :+ ClosingParenthesis, tail)
					}
					
					case IntegerLiteral(_) => {
						lastWholeTermIndex = acc.length						
						
						acc = acc :+ head
						
						if(open){
							acc = acc :+ ClosingParenthesis
							open = false
						}
					}
					
					case Operator(kind) => {
						kind match {
							case precedenceOperator.kind => {
								val before = acc.slice(0, lastWholeTermIndex)
								val after = acc.slice(lastWholeTermIndex, acc.length)
								
								open = true
								
								acc = before :+ OpeningParenthesis :++ after :+ head
							}
							
							case _ => {
								acc = acc :+ head
							}
						}
					}
					
					case _ => {
						println("unkown term", head)
						
						acc = acc :+ head
					}
				}
				
				if(tail.length == 0) return (acc, List())
				
				head = tail.head
				tail = tail.tail
			}while(true)
			
			(acc, List())
		}
		
		def eval(setCurrent:Double, terms:List[Term]):Tuple2[Double, List[Term]] = {
			var current = setCurrent
			
			if(terms.length == 0) return (current, List())
			
			var head = terms.head
			var tail = terms.tail
			
			var currentOp:Term = EmptyTerm
			
			do{
				head match {
					case OpeningParenthesis => {
						val result = eval(current, tail)
						
						tail = result._2
						
						current = currentOp.execute(current, result._1)						
					}
					
					case ClosingParenthesis => {
						return (current, tail)
					}
					
					case IntegerLiteral(value) => {						
						current = currentOp.execute(current, value.toDouble)						
					}
					
					case Operator(_) => {
						currentOp = head
					}
					
					case _ => {
						println("unkown term")
					}
				}				
				
				if(tail.length == 0){
					return (current, List())
				}
				
				head = tail.head
				tail = tail.tail
			}while(true)
			
			(current, List())
		}
		
		def evaluate(expressionStr:String, usePrecedence:Boolean = true):Double = {
			val terms = ExpressionParser.toTerms(expressionStr)		
			
			val grouped:List[Term] = if(usePrecedence) ExpressionParser.groupByPrecedence(ExpressionParser.Operator("+"), terms)._1 else terms
			
			val value = ExpressionParser.eval(0.0, grouped)._1
			
			value
		}
		
		def groupByPrecedenceAsString(expressionStr:String) = {
			val terms = toTerms(expressionStr)
			
			val grouped = ExpressionParser.groupByPrecedence(ExpressionParser.Operator("+"), terms)._1
			
			termsToString(grouped)
		}
	}
	
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
