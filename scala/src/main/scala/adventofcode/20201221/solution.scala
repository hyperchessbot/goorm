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
						
			if(input._2 == 0){
				println(foods.map(_.ingredients).flatten.count(safeIngredients.contains(_)))
			}
			
			if(input._2 == 1){
				val canonicalMap = scala.collection.mutable.Map[String, String]()
				
				def removeOnce():Boolean = {
					for((allergen, ingredients) <- canBeIn){
						if(ingredients.size == 1){
							val ingredient = ingredients.head
							canonicalMap.update(ingredient, allergen)
							canBeIn -= allergen
							for((testAllergen, testIngredients) <- canBeIn){
								canBeIn.update(testAllergen, testIngredients - ingredient)
							}
							return true
						}
					}
					
					false
				}
				
				while(removeOnce()){}
				
				println(canonicalMap.toSeq.sortBy(_._2).map(_._1).mkString(","))
			}
		}
	}
	
	def solve():Unit = {
		val delim = "----------------------"
		
		for(input <- List(("example", 0), ("input", 0), ("example", 1), ("input", 1))) time(input._1 + " version " + input._2.toString(), "ms", {    
			println (s"$delim\n$packageDate ${input._1} version ${input._2} processing")
			
			solveInput(input)
		})
		
		println(delim)
	}
}
