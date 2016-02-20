package planook

import planook.RequestModule.{Meal, MealRequest}

import scala.collection.breakOut

object RecipeFinder extends RecipeModule with JsonRecipe {

  // tip: create recipe json files here: http://www.objgen.com/json
  lazy val breakfasts: Seq[Recipe] = parseJsonFiles("breakfast")
  lazy val dinners: Seq[Recipe] = parseJsonFiles("dinner")
  lazy val lunchAndDinner: Seq[Recipe] = parseJsonFiles("lunch-and-dinner")
  lazy val sandwiches: Seq[Recipe] = parseJsonFiles("sandwiches")
  lazy val soups: Seq[Recipe] = parseJsonFiles("soups")
  lazy val weekendEntrees: Seq[Recipe] = parseJsonFiles("weendend-entrees")
  lazy val weekendBreakfasts: Seq[Recipe] = parseJsonFiles("weekend-breakfast")

  def findRecipes(mealRequest: MealRequest): Seq[Recipe] = {
    import Meal._
    import mealRequest._
    val db = mealType match {
        case Breakfast        => breakfasts
        case Dinner           => dinners
        case LunchOrDinner    => lunchAndDinner
        case Sandwich         => sandwiches
        case Soup             => soups
        case WeekendBreakfast => weekendBreakfasts
        case WeekendEntree    => weekendEntrees
    }
    randomRecipes(num, db) map {
      _.getPoritions(people * days)
    }
  }

  import IngredientUnit._

  def shoppingList(recipes: Seq[Recipe], printObvious: Boolean = false): Iterable[Ingredient] = {
    val ingredients = recipes flatMap { _.ingredients }
    val map = ingredients.foldLeft(Map.empty[String, Amount]) {
      case (oldMap, ingr) =>
        val name = ingr.name
        oldMap get name match {
          case Some(a@(unit, quantity)) =>
            unifyUnits(a, (ingr.unit, ingr.quantity)) match {
              case Some((newUnit, quantity1, quantity2)) =>
                oldMap + (normalize(name) -> (newUnit, quantity1 + quantity2))
              case None                                  =>
                throw new UnsupportedOperationException(s"can't unify $quantity $unit and ${ingr.quantity} ${ingr.unit} of $name")
            }
          case None                   =>
            oldMap + (normalize(name) -> (ingr.unit, ingr.quantity))
        }
    }
    val toBuy = map collect {
      case (name, (unit, quantity)) if printObvious || !(Data.OBVIOUS contains name) =>
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
    Data.produceWithPlural contains normalizeForSearch(str)

  def normalize(str: String): String =
    str.trim.toLowerCase.replaceAll("[-/]", " ")

  def normalizeForSearch(str: String): String =
    normalize(str) replaceFirst("^fresh ", "")

  /**
    * Chooses `n` different random recipes from a data base
    */
  private[this] def randomRecipes(n: Int, db: Seq[Recipe]): Seq[Recipe] =
    (scala.util.Random.shuffle(db.indices.toList) take n map {
      db(_)
    })(breakOut)
}
