package planook

object RequestModule {

  object Meal extends Enumeration {
    type Meal = Value
    val Breakfast = Value("b")
    val WeekendBreakfast = Value("B")
    val LunchOrDinner = Value("l")
    val Dinner = Value("d")
    val WeekendEntree = Value("E")
    val Party = Value("p")

    def fullName(meal: Meal): String =
      meal match {
        case Breakfast        => "breakfast"
        case WeekendBreakfast => "weekend breakfast"
        case LunchOrDinner    => "lunch/dinner"
        case Dinner           => "dinner"
        case WeekendEntree    => "weekend lunch/dinner"
        case Party            => "party"
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
