package planook

import java.nio.file.{Path, Files, Paths}

import argonaut.{CursorHistory, Parse, Json}

import scala.collection.JavaConversions._
import scala.collection.breakOut

trait JsonRecipe extends RecipeModule {

  def parseJsonFiles(db: String): Seq[Recipe] = {
    val files = Files.newDirectoryStream(Paths.get(s"db/$db"))
    (files flatMap jsonRecipe)(breakOut)
  }

  def jsonRecipe(path: Path): Option[Recipe] = {
    val string = new String(Files.readAllBytes(path))
    Parse.decodeOption[Recipe](string)
  }
}
