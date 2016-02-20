package planook

import planook.RecipeFinder._

trait PrettyPrint {

  def printLong(recipes: Seq[Recipe]): Unit = {
    val string =
      s"""
       |${sort(recipes) mkString "\n***\n"}
       |
       |${shoppingListString(recipes)}
        """.stripMargin
    println(string)
  }

  def printShort(recipes: Seq[Recipe]): Unit = {
    val string =
      s"""
         |${sort(recipes) map { _.shortString } mkString "\n\n"}
         |
         |${shoppingListString(recipes)}
       """.stripMargin
    println(string)
  }

  private[this] def shoppingListString(recipes: Seq[Recipe]): String =
    s"""
       |Shopping List:
       |${shoppingList(recipes) mkString "\n"}
     """.stripMargin

  private[this] def sort(recipes: Seq[Recipe]): Seq[Recipe] =
    recipes sortBy {
      _.meal
    }
}
