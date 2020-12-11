package object table{
	class Table(setWidth:Int, setHeight:Int, setInit:String = " "){
		val width = setWidth
		val height = setHeight
		val init = setInit
		
		val size = width * height
		
		var rep:Array[String] = Array.fill(size){init}
		
		val delim = Array.fill(width ){"━"}.mkString
		val delimTop = "┏" + delim + "┓"
		val delimBottom = "┗" + delim + "┛"
		
		override def toString():String = delimTop + "\n" + (0 until height).map(y => "┃" + rep.slice(y * width, ( y + 1 ) * width).mkString.replaceAll(" ", "·") + "┃").mkString("\n") + "\n" + delimBottom
		
		def okXY(x:Int, y:Int):Boolean = ( x >= 0) && ( x < width) && ( y >= 0 ) && ( y < height)
		
		def okXYd(x:Int, y:Int, dx:Int, dy:Int):Boolean = (!((dx == 0) && (dy == 0))) && okXY(x + dx, y + dy)
		
		def posXY(x:Int, y:Int):Int = y * width + x
		
		def getXY(x:Int, y:Int):String = {
			rep(posXY(x, y))
		}
		
		def getXYd(x:Int, y:Int, dx:Int, dy:Int):String = {
			rep(posXY(x + dx, y + dy))
		}
		
		def setXY(x:Int, y:Int, value:String):Unit = {
			rep(posXY(x, y)) = value
		}
		
		def getNeighbours(x:Int, y:Int):List[String] = (for(dy <- List(-1, 0, 1); dx <- List(-1, 0, 1) if okXYd(x, y, dx, dy)) yield rep(posXY(x + dx, y + dy))).toList
		
		override def clone():Table = {
			val table = new Table(width, height)
			
			table.rep = rep.slice(0, rep.length)
			
			table
		}
		
		def countInNeighbours(x:Int, y:Int, str:String):Int = getNeighbours(x, y).count(_ == str)
		
		def countInAll(str:String):Int = rep.count(_ == str)
		
		def equal(table: Table):Boolean = rep.sameElements(table.rep)
	}
	
	object Table{
		def apply(lines:List[String]):Table = {
			val temp = lines.map(_.split(""))
			
			val table = new Table(temp.map(_.length).max, lines.length)
			
			table.rep = temp.flatten.toArray
			
			table
		}
	}
}
