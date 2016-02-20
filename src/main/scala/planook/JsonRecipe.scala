package planook

import java.nio.file.{Files, Path, Paths}

import argonaut.{CursorHistory, Parse}

import scala.collection.JavaConversions._
import scala.collection.breakOut
import scalaz.\/

trait JsonRecipe extends RecipeModule {

  def parseJsonFiles(db: String): Seq[OriginalRecipe] = {
    val path: Path = Paths.get(s"src/main/resources/db/$db")
    val files = Files.newDirectoryStream(path)
    (files map jsonRecipe)(breakOut)
  }

  def jsonRecipe(path: Path): OriginalRecipe = {
    val string = new String(Files.readAllBytes(path))
    val decoded: \/[\/[String, (String, CursorHistory)], OriginalRecipe] = Parse.decode[OriginalRecipe](string)
    if (decoded.isRight) decoded.toOption.get
    else throw new UnsupportedOperationException(
      s"""invalid recipe file ${path.getFileName}
         |error message: ${decoded.toEither.left.toString}""".stripMargin)
  }
}
