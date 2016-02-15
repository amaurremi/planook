package planook

import planook.RequestModule.{Meal, MealRequest}

import scala.collection.breakOut

object RecipeFinder extends RecipeModule {

  // tip: create recipes here: http://www.objgen.com/json
  // every recipe is stored in a "normalized" format, for one portion
  val breakfasts: Seq[Recipe] = ???
  val entrees: Seq[Recipe] = ???

  def findRecipes(mealRequest: MealRequest): Seq[Recipe] = {
    import mealRequest._
    val db = if (mealType == Meal.Breakfast) breakfasts else entrees
    randomRecipes(num, db) map {
      _.getPoritions(people * days)
    }
  }

  /**
    * Chooses `n` different random recipes from a data base
    */
  private[this] def randomRecipes(n: Int, db: Seq[Recipe]): Seq[Recipe] =
    (scala.util.Random.shuffle(1 to db.length) take n map {
      db(_)
    })(breakOut)
}
