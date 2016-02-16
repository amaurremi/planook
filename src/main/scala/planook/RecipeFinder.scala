package planook

import planook.RequestModule.{Meal, MealRequest}

import scala.collection.breakOut
import scala.collection.immutable.Iterable

object RecipeFinder extends RecipeModule with JsonRecipe {

  // tip: create recipe json files here: http://www.objgen.com/json
  lazy val breakfasts: Seq[Recipe] = parseJsonFiles("breakfast")
  lazy val weekendBreakfasts: Seq[Recipe] = parseJsonFiles("weekend-breakfast")
  lazy val entrees: Seq[Recipe] = parseJsonFiles("entree")

  def findRecipes(mealRequest: MealRequest): Seq[Recipe] = {
    import Meal._
    import mealRequest._
    val db = mealType match {
        case Breakfast        => breakfasts
        case WeekendBreakfast => weekendBreakfasts
        case _                => entrees
    }
    randomRecipes(num, db) map {
      _.getPoritions(people * days)
    }
  }

  import IngredientUnit._

  def shoppingList(recipes: Seq[Recipe]): Iterable[Ingredient] = {
    val ingredients = recipes flatMap { _.ingredients }
    val map = ingredients.foldLeft(Map.empty[String, Amount]) {
      case (oldMap, ingr) =>
        val name = ingr.name
        oldMap get name match {
          case Some(a@(unit, quantity)) =>
            val (newUnit, quantity1, quantity2) = unifyUnits(a, (ingr.unit, ingr.quantity))
            oldMap + (normalize(name) -> (newUnit, quantity1 + quantity2))
          case None                   =>
            oldMap + (normalize(name) -> (ingr.unit, ingr.quantity))
        }
    }
    val toBuy = map map {
      case (name, (unit, quantity)) =>
        Ingredient(name, quantity, unit, None)
    }
    toBuy.toList sortWith {
      (i1, i2) =>
        val prod1 = isProduce(i1.name)
        val prod2 = isProduce(i2.name)
        if (prod1 && !prod2)
          true
        else if (prod1 && prod2 || !prod1 && !prod2)
          i1.name < i2.name
        else false
    }
  }

  private[this] def isProduce(str: String): Boolean =
    Data.produceWithPlugal contains normalize(str)

  def normalize(str: String): String = // todo better
    str.trim.toLowerCase.replaceAll("-/", " ")

  /**
    * Chooses `n` different random recipes from a data base
    */
  private[this] def randomRecipes(n: Int, db: Seq[Recipe]): Seq[Recipe] =
    (scala.util.Random.shuffle(db.indices.toList) take n map {
      db(_)
    })(breakOut)
}
