package object utils{	
	def getLinesOf(path:String):List[String] = scala.io.Source.fromFile(path).getLines().toList	
	
	def p(content:String):Unit = println(content)
}
