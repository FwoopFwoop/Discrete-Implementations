object BaseConversion {

  def isNumeric(n: String): Boolean = {
    n.filter(c => !c.isDigit).isEmpty
  }

  def isValidNumber(n: String, base: Int): Boolean = {
    isNumeric(n) && n.filter(c => c.toString.toInt >= base).isEmpty
  }

  def addNumbers(a: String, b: String, base: Int): Option[String] = {
    if (isValidNumber(a, base) && isValidNumber(b, base)) {
      def last(n: String): Int = if (n.isEmpty) 0 else n.last.toString.toInt

      def next(n: String): String = if (n.isEmpty) "" else n.substring(0, n.length - 1)

      def carryAdder(a: String, b: String, carryDigit: Int, result: String): String = {
        if (a.isEmpty && b.isEmpty) {
          result
        } else {
          val value = last(a) + last(b) + carryDigit
          val digit = if (value >= base) (value - base).toString else value.toString
          val newCarry = if (value >= base) 1 else 0

          carryAdder(next(a), next(b), newCarry, digit + result)
        }
      }

      Some(carryAdder(a, b, 0, ""))
    } else None
  }
  
  def toBase10(n: String, base: Int): Option[Int] = {
    if (isValidNumber(n, base)) {
      def accumulator(n: String, place: Int, result: Int): Int = {
        if (n.isEmpty) {
          result
        } else {
          val n1 = n.substring(0, n.length - 1)
          val value = n.last.toString.toInt * scala.math.pow(base, place).toInt

          accumulator(n1, place + 1, result + value)
        }
      }

      Some(accumulator(n, 0, 0))
    } else None
  }

  def changeMethod(n: Int, base: Int): String = {
    def buildNumber(n: Int, lastPower: Int, converted: String): String = {
      if (lastPower == 1) converted
      else {
        val newPower = lastPower / base
        val digit = n / newPower
        val rest = n - digit * newPower

        buildNumber(rest, newPower, converted + digit.toString)
      }
    }

    def startingPower(n: Int, power: Int, base: Int): Int = {
      if (power > n) power else startingPower(n, power * base, base)
    }

    buildNumber(n, startingPower(n, 1, base), "")
  }

  def remainderMethod(n: Int, base: Int): String = {
    if (n == 0) "" else remainderMethod(n / base, base) + (n % base).toString
  }

  def main(args: Array[String]): Unit = {
    //TODO get a real testing library
    println(toBase10("1100", 2))

    val sum = addNumbers(changeMethod(360, 7), changeMethod(60, 7), 7) match {
      case None => ""
      case Some(s: String) => s
    }

    println(toBase10(sum, 7))
  }
}