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
      * mpdn,
      * where m ∈ {b, l, d} for breakfast, lunch, or dinner
      *       p ∈ ℕ is the number of people who will eat the meal
      *       d ∈ ℕ is the number of days for which the meal is needed
      *       n ∈ ℕ is optional and represents the number of times we want to cook that type of meal during D
      */
    implicit val mealRequestRead: scopt.Read[MealRequest] =
      Read.reads(
        s => {
          val length = s.length
          lazy val array = s.toCharArray map { _. toString }
          if (length == 3) {
            val Array(m, p, d) = array
            MealRequest(Meal.withName(m), Integer.valueOf(p), Integer.valueOf(d), 1)
          } else if (length == 4) {
            val Array(m, p, d, n) = array
            MealRequest(Meal.withName(m), Integer.valueOf(p), Integer.valueOf(d), Integer.valueOf(n))
          } else throw new UnsupportedOperationException("Illegal meal request format")
        })

    head("planook")
    arg[MealRequest]("<meal-request>...") unbounded() action {
      (mealRequest, opts) =>
        opts add mealRequest
    } text """A meal request should be specified in a string as follows:
              | mpdn,
              | where m ∈ {b, l, d} for breakfast, lunch, or dinner
              |       p ∈ ℕ is the number of people who will eat the meal
              |       d ∈ ℕ is the number of days for which the meal is needed
              |       n ∈ ℕ is optional and represents the number of times we want to cook that type of meal during D"""
  }
}
