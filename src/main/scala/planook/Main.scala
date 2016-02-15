package planook

import planook.RequestModule.{Meal, MealRequest}
import scopt.{OptionParser, Read}

object Main {

  def main(args: Array[String]) = {
    parser.parse(args, CmdOptions()) foreach { options =>
      val recipeSeqs = options.requests map RecipeFinder.findRecipes
      println(recipeSeqs)
    }
  }

  case class CmdOptions (
    requests: Seq[MealRequest] = Seq.empty[MealRequest]
  ) {
    def add(request: MealRequest): CmdOptions = CmdOptions(request +: requests)
  }

  val parser = new OptionParser[CmdOptions]("scopt") {

    /**
      * A meal request should be specified in a string as follows:
      * XDCP,
      * where X ∈ {b, l, d} for breakfast, lunch, or dinner
      *       D ∈ ℕ is the number of days for which recipes are needed
      *       C ∈ ℕ is the number of times we want to cook that type of meal during D
      *       P ∈ ℕ is the number of people who will eat the meal
      */
    implicit val mealRequestRead: scopt.Read[MealRequest] =
      Read.reads(
        s => {
          if (s.length != 4) throw new UnsupportedOperationException("Illegal meal request format")
          val Array(m, d, c, p) = s.toCharArray map { _. toString }
          MealRequest(Meal.withName(m), Integer.valueOf(d), Integer.valueOf(c), Integer.valueOf(p))
        })

    head("planook")
    arg[MealRequest]("<meal-request>...") unbounded() action {
      (mealRequest, opts) =>
        opts add mealRequest
    } text """A meal request should be specified in a string as follows:
              | XDCP,
              | where X ∈ {b, l, d} for breakfast, lunch, or dinner
              |       D ∈ ℕ is the number of days for which recipes are needed
              |       C ∈ ℕ is the number of times we want to cook that type of meal during D
              |       P ∈ ℕ is the number of people who will eat the meal """
  }
}
