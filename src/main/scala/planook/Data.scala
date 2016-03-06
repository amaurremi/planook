package planook

import scala.collection.breakOut

object Data {

  val PRODUCE = Set(
    "apple", "pear", "peach", "nectarine", "strawberry", "grape", "celery", "spinach", "basil",
    "tomato", "bell pepper", "cucumber", "cherry tomato", "blueberry", "pepper", "snap pea", "potato",
    "raspberry", "lettuce", "kale", "plum", "squash", "winter squash", "tangerine", "lemon", "orange",
    "mandarine", "clementine", "carrot", "broccoli", "sugar pea", "green onion", "banana", "watermelon",
    "honey melon", "mushroom", "sweet potato", "cauliflower", "cantaloupe", "grapefruit", "eggplant",
    "aubergine", "kiwi", "papaya", "mango", "onion", "asparagus", "red onion", "cabbage", "red cabbage",
    "pineapple", "sweet corn", "avocado", "arugula", "beet", "blackberry", "baby bok choy", "bok choy",
    "baby spinach", "brussels sprout", "green cabbage", "savoy cabbage", "chard", "cilantro", "chili",
    "chili pepper", "dill", "fennel", "garlic", "ginger", "granny smith apple", "garlic clove", "red grapefruit",
    "mint", "oregano", "rosemary", "thyme", "sage", "tarragon", "chives", "green kale", "leek", "lemon grass",
    "lemongrass", "lime", "micro green", "shiitake", "cherry", "paprika", "red paprika", "green paprika",
    "cremini mushroom", "shiitake mushroom", "portobello mushroom", "okra", "sweet onion", "blood orange",
    "parsley", "flat leaf parsley", "italian parsley", "curly parsley", "asian pear", "anjou pear", "green pepper",
    "red pepper", "jalapeno", "jalapeno pepper", "orange pepper", "poblano pepper", "russet potato",
    "fingerling potato", "radicchio", "radish", "iceberg", "iceberg lettuce", "iceberg salad", "scallion", "shallot",
    "zucchini", "grape tomato", "watercress", "yuca", "red bell pepper", "sprout", "curly kale", "green bean",
    "lamb's lettuce", "berry", "yellow bell pepper", "green bell pepper", "spring onion", "pomegranate",
    "pomegranate seed", "pomegranate aril", "celery stalk", "red hot chili pepper", "red hot chilli pepper", "snow pea",
    "salad leaves", "romaine lettuce", "spring mix salad", "small cucumber"
  )

  val OBVIOUS = Set(
    "water", "salt", "pepper", "ground pepper", "sea salt", "coarse sea salt", "freshly ground pepper", "olive oil",
    "oil", "sugar", "fine sea salt", "balsamic vinegar", "black peppercorns"
  )

  lazy val produceWithPlural: Set[String] =
    (PRODUCE flatMap {
      p =>
        val  lastIndex = p.length - 1
        val (prefix, last) = (p substring (0, lastIndex), p substring lastIndex)
        val plural = last match {
          case "o"                                          => Seq(prefix + "oes", prefix + "os")
          case "y" if !(p matches ".*[aeiou]y$")            => Seq(prefix + "ies")
          case "s"                                          => Seq(p)
          case _ if (p endsWith "ch") || (p endsWith "sh")  => Seq(p + "es")
          case _                                            => Seq(p + "s")
         }
        p +: plural
    })(breakOut)
}
