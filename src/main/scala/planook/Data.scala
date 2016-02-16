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
    "baby spinach", "brussels sprouts", "green cabbage", "savoy cabbage", "chard", "cilantro", "chili",
    "chili pepper", "dill", "fennel", "garlic", "ginger", "granny smith apple", "garlic clove", "grapefruit",
    "mint", "oregano", "rosemary", "thyme", "sage", "tarragon", "chives", "green kale", "leek", "lemon grass",
    "lemongrass", "lime", "micro green",
    "cremini mushroom", "shiitake mushroom", "portobello mushroom", "okra", "sweet onion", "blood orange",
    "parsley", "flat leaf parsley", "italian parsley", "curly parsley", "asian pear", "anjou pear", "green pepper",
    "red pepper", "jalapeno", "jalapeno pepper", "orange pepper", "poblano pepper", "russet potato",
    "fingerling potato", "radicchio", "radish", "iceberg", "iceberg lettuce", "iceberg salad", "scallion", "shallot",
    "zucchini", "grape tomato", "watercress", "yuca"
  )

  lazy val produceWithPlural: Set[String] =
    (PRODUCE flatMap {
      p =>
        val  lastIndex = p.length - 1
        val (prefix, last) = (p substring (0, lastIndex), p substring lastIndex)
        val plural = last match {
          case "o" => prefix + "oes"
          case "y" if !(p matches ".*[aeiou]y$") => prefix + "ies"
          case "s" => p
          case _ if (p endsWith "ch") || (p endsWith "sh")  => p + "es"
          case _   => p + "s"
         }
        Seq(p, plural)
    })(breakOut)
}
