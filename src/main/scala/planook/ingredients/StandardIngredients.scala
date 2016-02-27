package planook.ingredients

trait StandardIngredients extends IngredientUnits {

  import IngredientUnit._

  case class StandardIngredient private[StandardIngredients] (
    name: String,
    unit: IngredientUnit,
    gToMl: Double,
    mlToG: Double
  )

  object StandardIngredient {

    def apply(name: String): Option[StandardIngredient] = None
  }
}
