package planook

object RequestModule {

  object Meal extends Enumeration {
    type Meal = Value
    val Breakfast = Value("b")
    val Lunch = Value("l")
    val Dinner = Value("d")
  }

  import Meal._

  case class MealRequest(
    mealType: Meal,
    days: Int,
    cookingTimes: Int,
    people: Int
  )
}
