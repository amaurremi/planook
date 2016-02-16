package planook

import argonaut.Argonaut._
import argonaut.DecodeJson
import org.joda.time.{Period, Duration}

trait RecipeModule {

  object IngredientUnit extends Enumeration {
    type IngredientUnit = Value
    val Gram = Value("g")
    val MilliLiter = Value("ml")
    val Item = Value("item")
    val Cup = Value("cup")
    val TableSpoon = Value("tbs")
    val TeaSpoon = Value("ts")
  }

  import IngredientUnit._

  case class Ingredient(
   name: String,
   quantity: Double,
   unit: IngredientUnit,
   state: Option[String]
  ) {
    def multiply(n: Int): Ingredient = {
      val newQuantity: Double = n * quantity
      if (unit == TeaSpoon && newQuantity % 3 == 0)
        copy(quantity = newQuantity / 3, unit = TableSpoon)
      else copy(quantity = newQuantity)
    }
  }

  implicit def IngredientDecodeJson: DecodeJson[Ingredient] =
    DecodeJson(
      c =>
        for {
          n <- (c -- "name").as[String]
          q <- (c -- "quantity").as[Double]
          u <- (c -- "unit").as[String]
          s <- (c -- "state").as[Option[String]]
        } yield Ingredient(n, q, IngredientUnit.withName(u), s)
    )

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

  implicit def RecipeDecodeJson: DecodeJson[Recipe] =
    DecodeJson(
      c =>
        for {
          n <- (c -- "name").as[String]
          i <- (c -- "ingredients").as[List[Ingredient]]
          t <- (c -- "time").as[Int]
          p <- (c -- "portions").as[Int]
          u <- (c -- "url").as[String]
          d <- (c -- "description").as[String]
        } yield Recipe(n, i, new Period(0, t, 0, 0), p, u, d)
    )
}
