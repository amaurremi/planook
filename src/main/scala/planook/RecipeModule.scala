package planook

import java.text.DecimalFormat

import argonaut.Argonaut._
import argonaut.DecodeJson
import org.joda.time.Period
import org.joda.time.format.PeriodFormatterBuilder
import planook.RequestModule.Meal.Meal

trait RecipeModule {

  object IngredientUnit extends Enumeration {
    type IngredientUnit = Value
    type Amount = (IngredientUnit, Double)

    val Gram = Value("g")
    val Pound = Value("lb")
    val MilliLiter = Value("ml")
    val Pint = Value("pint")
    val Ounce = Value("oz")
    val Item = Value("item")
    val Cup = Value("cup")
    val TableSpoon = Value("tbs")
    val TeaSpoon = Value("ts")
    val Can = Value("can")
    val Slice = Value("slice")
    val Handful = Value("handful")
    val Bunch = Value("bunch")
    val Leaf = Value("leaf")

    def isMass(unit: IngredientUnit): Boolean =
      Seq(Gram, Pound, Ounce) contains unit

    def isVolume(unit: IngredientUnit): Boolean =
      Seq(MilliLiter, Cup, TableSpoon, TeaSpoon, Can, Pint) contains unit

    def unifyUnits(amount1: Amount, amount2: Amount): Option[(IngredientUnit, Double, Double)] = {
      val (u1, q1) = amount1
      val (u2, q2) = amount2
      if (u1 == u2)
        Some(u1, q1, q2)
      else if (isMass(u1) && isMass(u2))
        Some(Gram, toGrams(u1, q1), toGrams(u2, q2))
      else if (isVolume(u1) && isVolume(u2))
        Some(MilliLiter, toMl(u1, q1), toMl(u2, q2))
      else None
    }

    def toGrams(unit: IngredientUnit, quantity: Double): Double = {
      assert(isMass(unit), s"can't convert $unit to grams")
      val mult = unit match {
        case Gram  => 1
        case Pound => 454
        case Ounce => 28
      }
      mult * quantity
    }

    def toMl(unit: IngredientUnit, quantity: Double): Double = {
      assert(isVolume(unit), s"can't convert $unit to ml")
      val mult = unit match {
        case MilliLiter => 1
        case Cup        => 237
        case TableSpoon => 15
        case TeaSpoon   => 5
        case Can        => 400
        case Pint       => 473
      }
      mult * quantity
    }
  }

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

  case class OriginalRecipe(
   name: String,
   ingredients: Seq[Ingredient],
   otherIngredients: Seq[String],
   cookingTime: Period,
   portions: Int,
   url: String,
   description: String
  )

  implicit def RecipeDecodeJson: DecodeJson[OriginalRecipe] =
    DecodeJson(
      c =>
        for {
          m <- (c --\ "meal").as[String]
          n <- (c --\ "recipe-name").as[String]
          i <- (c --\ "in").as[List[Ingredient]]
          o <- (c --\ "other-ingredients").as[Option[List[String]]]
          t <- (c --\ "time").as[Int]
          p <- (c --\ "portions").as[Int]
          u <- (c --\ "url").as[String]
          d <- (c --\ "description").as[String]
        } yield OriginalRecipe(n, i, o.toSeq.flatten, new Period(0, t, 0, 0), p, u, d)
    )

  case class CreatedRecipe private[RecipeModule] (
    originalRecipe: OriginalRecipe,
    newPortions: Int,
    newIngredients: Seq[Ingredient],
    mealType: Meal,
    id: Int
  ) {

    import originalRecipe._

    def shortString: String =
      s"""
         |$name
         |$newPortions portions
         |$url
       """.stripMargin

    override def toString = {
      val formatter = new PeriodFormatterBuilder()
        .appendHours().appendSuffix(" h ").appendMinutes().appendSuffix(" min").toFormatter
      s"""
         |$name
         |
         |Ingredients:
         |${newIngredients mkString "\n"}
         |${otherIngredients mkString "\n"}
         |
         |Cooking time: ${formatter print cookingTime.normalizedStandard}
         |Portions: $newPortions
         |URL: $url
         |
         |Instructions:
         |$description
         |""".stripMargin
    }
  }

  object CreatedRecipe {

    def apply(originalRecipe: OriginalRecipe, portions: Int, mealType: Meal, id: Int): CreatedRecipe = {
      val newIngredients = originalRecipe.ingredients map {
        _ multiply (portions.toDouble / originalRecipe.portions)
      }
      CreatedRecipe(originalRecipe, portions, newIngredients, mealType, id)
    }
  }
}
