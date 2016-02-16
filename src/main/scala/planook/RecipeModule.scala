package planook

import argonaut.Argonaut._
import argonaut.DecodeJson
import org.joda.time.Period
import org.joda.time.format.PeriodFormatterBuilder

trait RecipeModule {

  object IngredientUnit extends Enumeration {
    type IngredientUnit = Value
    val Gram = Value("g")
    val Pound = Value("lb")
    val MilliLiter = Value("ml")
    val Ounce = Value("oz")
    val Item = Value("item")
    val Cup = Value("cup")
    val TableSpoon = Value("tbs")
    val TeaSpoon = Value("ts")
    val Can = Value("can")
    val Slice = Value("slice")
  }

  import IngredientUnit._

  case class Ingredient(
   name: String,
   quantity: Double,
   unit: IngredientUnit,
   state: Option[String]
  ) {
    def multiply(n: Int): Ingredient = {
      val newQuantity = n * quantity
      if (unit == TeaSpoon && newQuantity % 3 == 0)
        copy(quantity = newQuantity / 3, unit = TableSpoon)
      else copy(quantity = newQuantity)
    }

    override def toString = {
      val unitStr = if (unit == Item) "" else unit.toString + " "
      s"$quantity $unitStr$name" + (if (state.isDefined) s", ${state.get}" else "")
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

  case class Recipe(
   name: String,
   ingredients: Seq[Ingredient],
   otherIngredients: Seq[String],
   cookingTime: Period,
   portions: Int,
   url: String,
   description: String
  ) {

    def getPoritions(n: Int): Recipe = {
      val newIngredients = ingredients map {
        _ multiply (n / portions)
      }
      copy(ingredients = newIngredients, portions = n)
    }

    override def toString = {
      val formatter = new PeriodFormatterBuilder()
        .appendHours().appendSuffix(" h ").appendMinutes().appendSuffix(" min").toFormatter
      s"""
          |$name
          |
          |Ingredients:
          |${ingredients mkString "\n"}
          |${otherIngredients mkString "\n"}
          |
          |Cooking time: ${formatter print cookingTime.normalizedStandard}
          |Portions: $portions
          |URL: $url
          |
          |Instructions:
          |$description
          |""".stripMargin
      }
  }

  implicit def RecipeDecodeJson: DecodeJson[Recipe] =
    DecodeJson(
      c =>
        for {
          n <- (c --\ "recipe-name").as[String]
          i <- (c --\ "in").as[List[Ingredient]]
          o <- (c --\ "other-ingredients").as[Option[List[String]]]
          t <- (c --\ "time").as[Int]
          p <- (c --\ "portions").as[Int]
          u <- (c --\ "url").as[String]
          d <- (c --\ "description").as[String]
        } yield Recipe(n, i, o.toSeq.flatten, new Period(0, t, 0, 0), p, u, d)
    )
}
