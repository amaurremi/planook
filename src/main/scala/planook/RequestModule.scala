package planook

object RequestModule {

  object Meal extends Enumeration {
    type Meal = Value
    val Breakfast = Value("b")
    val WeekendBreakfast = Value("B")
    val LunchOrDinner = Value("l")
    val Dinner = Value("d")
    val Sandwich = Value("s")
    val Soup = Value("p")
    val WeekendEntree = Value("E")

    def fullName(meal: Meal): String =
      meal match {
        case Breakfast        => "breakfast"
        case WeekendBreakfast => "weekend breakfast"
        case LunchOrDinner    => "lunch/dinner"
        case Dinner           => "dinner"
        case Sandwich         => "sandwich"
        case Soup             => "soup"
        case WeekendEntree    => "weekend lunch/dinner"
      }
  }

  import Meal._

  case class MealRequest(
    mealType: Meal,
    people: Int,
    days: Int,
    num: Int
  )
}
