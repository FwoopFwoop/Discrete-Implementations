package main.scala

class BaseNumber(n: String, base: Int) {

  private def isValid(n: String, base: Int): Boolean = {
    n.filter(c => !c.isDigit).isEmpty && n.filter(c => c.toString.toInt >= base).isEmpty
  }

  def isValidNumber(): Boolean = {
    isValid(n, base)
  }

  private def addNumbers(a: String, b: String, base: Int): Option[String] = {
    if (isValid(a, base) && isValid(b, base)) {
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

  private def changeMethod(n: Int, base: Int): String = {
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

  private def remainderMethod(n: Int, base: Int): String = {
    if (n == 0) "" else remainderMethod(n / base, base) + (n % base).toString
  }

  //Returns the number as an integer in base 10 if the number is valid
  def toBase10(): Option[Int] = {
    if (isValid(n, base)) {
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

  override def toString: Option[String] = {
    if (isValidNumber) Some(n) else None
  }

  def +(b: BaseNumber): Option[BaseNumber] = {
    b.toBase10 match {
      case None => None
      case Some(b: Int) => addNumbers(n, remainderMethod(b, base), base) match {
        case None => None
        case Some(n: String) => Some(new BaseNumber(n, base))
      }
    }
  }
}