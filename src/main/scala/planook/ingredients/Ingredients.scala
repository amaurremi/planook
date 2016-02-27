package planook.ingredients

import java.text.DecimalFormat

import argonaut.DecodeJson

trait Ingredients extends IngredientUnits {

  import IngredientUnit._

  case class Ingredient(
   name: String,
   quantity: Double,
   unit: IngredientUnit,
   state: Option[String]
  ) {
    def multiply(n: Double): Ingredient = {
      val newQuantity = n * quantity
      if (unit == TeaSpoon && newQuantity % 3 == 0)
        copy(quantity = newQuantity / 3, unit = TableSpoon)
      else copy(quantity = newQuantity)
    }

    override def toString = {
      val unitStr = if (unit == Item) "" else unit.toString + " "
      val quantityStr = new DecimalFormat("#.#") format quantity
      s"$name, $quantityStr $unitStr" + (if (state.isDefined) s", ${state.get}" else "")
    }
  }

  implicit def IngredientDecodeJson: DecodeJson[Ingredient] =
    DecodeJson(
      c =>
        for {
          n <- (c --\ "n").as[String]         // name
          q <- (c --\ "q").as[Double]         // quantity
          u <- (c --\ "u").as[String]         // unit
          s <- (c --\ "s").as[Option[String]] // state
        } yield Ingredient(n, q, IngredientUnit.withName(u), s)
    )
}
