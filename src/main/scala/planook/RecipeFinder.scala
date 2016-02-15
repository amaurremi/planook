package planook

import planook.RequestModule.MealRequest

object RecipeFinder extends RecipeModule {

  def findRecipes(mealRequest: MealRequest): Seq[Recipe] = ???
}
