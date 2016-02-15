package planook

import com.github.nscala_time.time.Imports._

trait RecipeModule {

  sealed trait IngredientUnit
  case object Gram extends IngredientUnit
  case object MilliLiter extends IngredientUnit
  case object Item extends IngredientUnit

  case class Amount(quantity: Int, amount: IngredientUnit)

  case class Ingredient(
    name: String,
    amount: Amount
  )

  case class Recipe(
    ingredients: Seq[Ingredient],
    time: Period
  )
}

