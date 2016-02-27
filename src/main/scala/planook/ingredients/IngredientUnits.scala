package planook.ingredients

trait IngredientUnits {

  object IngredientUnit extends Enumeration {
    type IngredientUnit = Value
    type Amount = (IngredientUnit, Double)

    val Gram = Value("g")
    val Pound = Value("lb")
    val MilliLiter = Value("ml")
    val Pint = Value("pint")
    val Ounce = Value("oz")
    val Item = Value("item")
    val Cup = Value("cup")
    val TableSpoon = Value("tbs")
    val TeaSpoon = Value("ts")
    val Can = Value("can")
    val Slice = Value("slice")
    val Handful = Value("handful")
    val Bunch = Value("bunch")
    val Leaf = Value("leaf")

    def isMass(unit: IngredientUnit): Boolean =
      Seq(Gram, Pound, Ounce) contains unit

    def isVolume(unit: IngredientUnit): Boolean =
      Seq(MilliLiter, Cup, TableSpoon, TeaSpoon, Can, Pint) contains unit

    def toGrams(unit: IngredientUnit, quantity: Double): Double = {
      assert(isMass(unit), s"can't convert $unit to grams")
      val mult = unit match {
        case Gram  => 1
        case Pound => 454
        case Ounce => 28
      }
      mult * quantity
    }

    def toMl(unit: IngredientUnit, quantity: Double): Double = {
      assert(isVolume(unit), s"can't convert $unit to ml")
      val mult = unit match {
        case MilliLiter => 1
        case Cup        => 237
        case TableSpoon => 15
        case TeaSpoon   => 5
        case Can        => 400
        case Pint       => 473
      }
      mult * quantity
    }
  }
}
