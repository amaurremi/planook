package planook

import planook.RecipeFinder._

trait PrettyPrint {

  def printLong(recipes: Seq[CreatedRecipe]): Unit = {
    val string =
      s"""
       |${sort(recipes) mkString "\n***\n"}
       |${shoppingListString(recipes)}
        """.stripMargin
    println(string)
  }

  def printShort(recipes: Seq[CreatedRecipe]): Unit = {
    val string =
      s"""
         |${sort(recipes) map { _.shortString } mkString ""}
         |${shoppingListString(recipes)}
       """.stripMargin
    println(string)
  }

  private[this] def shoppingListString(recipes: Seq[CreatedRecipe]): String =
    s"""
       |Shopping List:
       |${shoppingList(recipes) mkString "\n"}
     """.stripMargin

  private[this] def sort(recipes: Seq[CreatedRecipe]): Seq[CreatedRecipe] =
    recipes sortBy {
      _.mealType
    }
}
