package adventofcode20201221

import scala.language.postfixOps
import utils._
import table._

object problem{	
	println(s"package name ${problem.getClass.getName.split("\\.")(0)}")
	
	val packageDate = "([0-9]+)".r.findFirstMatchIn(problem.getClass.getName).get
	
	val prefix = s"src/main/scala/adventofcode/$packageDate/"
	
	case class Food(foodStr:String){
		var ingredients = Set[String]()
		var allergens = Set[String]()
		
		foodStr match {
			case s"$ingredientsStr (contains $allergensStr)" =>  {
				ingredients = ingredientsStr.split(" ").toSet
				allergens = allergensStr.split(", ").toSet
			}
			case _ => println("invalid food", foodStr)
		}
		
		override def toString() = s"[Food $ingredients $allergens]"
	}
	
	def solveInput(input:Tuple2[String, Int]):Unit = {
		val lines = getLinesOf(s"$prefix${input._1}.txt")
			
		if(lines.length > 0){			
			val canBeIn = scala.collection.mutable.Map[String, Set[String]]()
			
			val allIngredients = scala.collection.mutable.Set[String]()
			val allergyIngredients = scala.collection.mutable.Set[String]()
			
			val foods = lines.map(Food(_)).toList
			
			foods.foreach(food => {
				allIngredients ++= food.ingredients				
				
				for(allergen <- food.allergens){
					canBeIn.updateWith(allergen)(_ match {
						case None => Some(food.ingredients)
						case Some(ingredients) => {							
							Some(ingredients.intersect(food.ingredients))
						}
					})
				}
			})
			
			for((_, ingredients) <- canBeIn){
				allergyIngredients ++= ingredients
			}
			
			val safeIngredients = allIngredients.diff(allergyIngredients)
			
			/*println("all ingredients")
			println(allIngredients)
			println("allergy ingredients")
			println(allergyIngredients)
			println("safe ingredients")
			println(safeIngredients)*/
			
			var cnt = 0
			
			for(food <- foods){
				for(ingredient <- food.ingredients){
					if(safeIngredients.contains(ingredient)) cnt += 1
				}
			}
			
			if(input._2 == 0){
				println(cnt)	
			}
			
			
		}
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0), ("example2", 1), ("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
