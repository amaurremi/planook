package planook

import argonaut.Argonaut._
import argonaut.DecodeJson
import org.joda.time.Period
import org.joda.time.format.PeriodFormatterBuilder
import planook.RequestModule.Meal
import planook.RequestModule.Meal.Meal
import planook.ingredients.{Ingredients, StandardIngredients}

trait RecipeModule extends Ingredients with StandardIngredients {

  import IngredientUnit._

  def unifyUnits(amount1: Amount, amount2: Amount, ingredient: String): Option[(IngredientUnit, Double, Double)] = {
    val (u1, q1) = amount1
    val (u2, q2) = amount2
    if (u1 == u2)
      Some(u1, q1, q2)
    else if (isMass(u1) && isMass(u2))
      Some(Gram, toGrams(u1, q1), toGrams(u2, q2))
    else if (isVolume(u1) && isVolume(u2))
      Some(MilliLiter, toMl(u1, q1), toMl(u2, q2))
    else unifyDifferentUnits(u1, q1, u2, q2, ingredient)
  }

  private[this] def unifyDifferentUnits(
    u1: IngredientUnit,
    q1: Double,
    u2: IngredientUnit,
    q2: Double,
    ingredient: String
  ): Option[(IngredientUnit, Double, Double)] =
    StandardIngredient(ingredient) map {
      standardIngredient => ???
    }

  case class OriginalRecipe(
   name: String,
   ingredients: Seq[Ingredient],
   otherIngredients: Seq[String],
   cookingTime: Period,
   portions: Int,
   minPortions: Int,
   url: String,
   description: String
  )

  implicit def RecipeDecodeJson: DecodeJson[OriginalRecipe] =
    DecodeJson(
      c =>
        for {
          n <- (c --\ "recipe-name").as[String]
          i <- (c --\ "in").as[List[Ingredient]]
          o <- (c --\ "other-ingredients").as[Option[List[String]]]
          t <- (c --\ "time").as[Int]
          p <- (c --\ "portions").as[Int]
          m <- (c --\ "portions-min").as[Option[Int]]
          u <- (c --\ "url").as[String]
          d <- (c --\ "description").as[String]
        } yield OriginalRecipe(n, i, o.toSeq.flatten, new Period(0, t, 0, 0), p, m getOrElse p, u, d)
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
         |${Meal.fullName(mealType)} for $newPortions portions
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
         |Meal type: ${Meal.fullName(mealType)}
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
