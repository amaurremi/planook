package planook

import argonaut._
import Argonaut._
import com.github.nscala_time.time.Imports._

trait RecipeModule {

  sealed trait IngredientUnit

  case object Gram extends IngredientUnit

  case object MilliLiter extends IngredientUnit

  case object Item extends IngredientUnit

  case object Cup extends IngredientUnit

  case object TableSpoon extends IngredientUnit

  case object TeaSpoon extends IngredientUnit

  case class Amount(quantity: Double, unit: IngredientUnit) {

    def multiply(n: Int): Amount = {
      val newQuantity: Double = n * quantity
      if (unit == TeaSpoon && newQuantity % 3 == 0)
        Amount(newQuantity / 3, TableSpoon)
      else copy(quantity = newQuantity)
    }
  }

  case class Ingredient(
   name: String,
   amount: Amount,
   state: Option[String]
  ) {
    def multiply(n: Int): Ingredient = {
      copy(amount = amount multiply n)
    }
  }

  implicit def IngredientCodecJson =
    casecodec3(Ingredient.apply, Ingredient.unapply)("name", "quantity", "unit")

  case class Recipe(
   name: String,
   ingredients: Seq[Ingredient],
   cookingTime: Period,
   portions: Int,
   url: String,
   description: String
  ) {

    def getPoritions(n: Int): Recipe = {
      val newIngredients = ingredients map {
        _ multiply (n / portions)
      }
      copy(ingredients = newIngredients)
    }
  }

  implicit def RecipeCodecJson =
    casecodec6(Recipe.apply, Recipe.unapply)("name", "ingredients", "time", "portions", "url", "description")
}
