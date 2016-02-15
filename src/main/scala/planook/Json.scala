package planook

trait Json extends RecipeModule {

  def parseJsonFiles(db: String): Seq[Recipe] = ???
}
