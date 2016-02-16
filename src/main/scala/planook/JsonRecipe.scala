package planook

import java.nio.file.{Path, Files, Paths}

import argonaut.Json

trait JsonRecipe extends RecipeModule {

  def parseJsonFiles(db: String): Seq[Recipe] = {
    val files = Files.newDirectoryStream(Paths.get(s"db/$db"))
    for {
      file <- files
      json = jsonRecipe(file)
    }
  }

  def jsonRecipe(path: Path): Json = ???
}
