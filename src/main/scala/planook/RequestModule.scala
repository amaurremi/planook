package planook

object RequestModule {

  object Meal extends Enumeration {
    type Meal = Value
    val Breakfast = Value("b")
    val LunchOrDinner = Value("l")
    val Dinner = Value("d")
    val Sandwich = Value("s")
    val Soup = Value("p")
    val WeekendBreakfast = Value("B")
    val WeekendEntree = Value("E")
  }

  import Meal._

  case class MealRequest(
    mealType: Meal,
    people: Int,
    days: Int,
    num: Int
  )
}
