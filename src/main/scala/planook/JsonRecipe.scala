package planook

import java.nio.file.{Files, Path, Paths}

import argonaut.Parse

import scala.collection.JavaConversions._
import scala.collection.breakOut

trait JsonRecipe extends RecipeModule {

  def parseJsonFiles(db: String): Seq[Recipe] = {
    val path: Path = Paths.get(s"src/main/resources/db/$db")
    val files = Files.newDirectoryStream(path)
    (files flatMap jsonRecipe)(breakOut)
  }

  def jsonRecipe(path: Path): Option[Recipe] = {
    val string = new String(Files.readAllBytes(path))
    Parse.decodeOption[Recipe](string)
  }
}
