package main.scala

class BaseNumber(n: String, base: Int) {

  //Constructor for a number in base 10
  def this(decimal: Int) {
    this(decimal.toString, 10)
  }

  //Constructor to create a number in a target base from the decimal value
  def this(decimal: Int, base: Int) {
    this(BaseNumber.convertFromDecimal(decimal, base), base)
  }

  //Produces the string value of the number if it is valid
  override def toString: Option[String] = {
    if (isValidNumber) Some(n) else None
  }

  //Returns true if this number is valid
  def isValidNumber(): Boolean = {
    BaseNumber.isValid(n, base)
  }

  //Returns the number as an integer in base 10 if the number is valid
  def toDecimal(): Option[Int] = {
    if (BaseNumber.isValid(n, base)) {
      def accumulator(n: String, place: Int, result: Int): Option[Int] = {
        if (n.isEmpty) {
          Some(result)
        } else {
          val n1 = n.substring(0, n.length - 1)
          val value = BaseNumber.toInt(n.last, base) match {
            case Some(value: Int) => value * scala.math.pow(base, place).toInt
            case None => None
          }
          value match {
            case None => None
            case Some(value: Int) => accumulator(n1, place + 1, result + value)
          }
        }
      }

      accumulator(n, 0, 0)
    } else None
  }


  //Returns a new BaseNumber with a different base, but the same value
  def changeBase(newBase: Int): Option[BaseNumber] = {
    toDecimal match {
      case None => None
      case Some(value: Int) => Some(new BaseNumber(BaseNumber.convertFromDecimal(value, newBase), newBase))
    }
  }


  //Implements addition operation (will produce None if either number is invalid)
  def +(b: BaseNumber): Option[BaseNumber] = {
    b.toDecimal match {
      case None => None
      case Some(b: Int) => addNumbers(n, BaseNumber.convertFromDecimal(b, base), base) match {
        case None => None
        case Some(n: String) => Some(new BaseNumber(n, base))
      }
    }
  }


  //Returns the sum of two numbers of the same base if they are both valid
  private def addNumbers(a: String, b: String, base: Int): Option[String] = {
    def carryAdder(a: String, b: String, carryDigit: Int, result: String): Option[String] = {
      if (a.isEmpty && b.isEmpty) {
        Some(result)
      } else {
        val value = BaseNumber.last(a) match {
          case None => None
          case Some(a: Int) => BaseNumber.last(b) match {
            case None => None
            case Some(b: Int) => Some(a + b + carryDigit)
          }
        }

        value match {
          case None => None
          case Some(value: Int) => {
            val digit = {
              if (value >= base) {
                BaseNumber.toString(value - base, base)
              } else {
                BaseNumber.toString(value, base)
              }
            }
            val newCarry = if (value >= base) 1 else 0

            carryAdder(BaseNumber.next(a), BaseNumber.next(b), newCarry, digit + result)
          }
        }
      }
    }

    carryAdder(a, b, 0, "")
  }

  //Returns the difference of two numbers of the same base if they are both valid,
  //and that the first is greater than the second
  def -(b: BaseNumber): Option[String] = {
    def borrowSubber(a: String, b: String, didBorrow: Boolean, result: String): Option[String] = {
      if (a.isEmpty && b.isEmpty) {
        Some(result)
      } else {
        BaseNumber.last(a) match {
          case None => None
          case Some(topDigitRaw: Int) => BaseNumber.last(b) match {
            case None => None
            case Some(bottomDigitValue: Int) => {
              val borrowAdjustedValue = topDigitRaw - (if (didBorrow) 1 else 0)
              val topDigitValue = if (borrowAdjustedValue < bottomDigitValue) {
                borrowAdjustedValue + base
              } else borrowAdjustedValue
              val borrowing = topDigitValue > borrowAdjustedValue
              BaseNumber.toString(topDigitValue - bottomDigitValue, this.base) match {
                case None => None
                case Some(digit: String) => borrowSubber(BaseNumber.next(a), BaseNumber.next(b),
                  borrowing, digit + result)
              }
            }
          }
        }
      }
    }
    this.toDecimal() match {
      case None => None
      case Some(aDec: Int) => b.toDecimal() match {
        case None => None
        case Some(bDec: Int) =>
          if (aDec >= bDec) {
            this.toString match {
              case None => None
              case Some(aVal: String) => b.changeBase(this.base) match {
                case None => None
                case Some(b: BaseNumber) => b.toString match {
                  case None => None
                  case Some(bVal: String) => borrowSubber(aVal, bVal, false, "")
                }
              }
            }
          } else None
      }
    }
  }


  private object BaseNumber {
    //Returns true if a number in string form and base make sense together
    def isValid(n: String, base: Int): Boolean = {
      //n.filter(c => !c.isDigit).isEmpty && n.filter(c => c.toString.toInt >= base).isEmpty
      n.filter(c => !isDigit(c, base)).isEmpty && base > 1 && base <= 36
    }

    //Returns true if the char is a digit in this base
    def isDigit(c: Char, base: Int): Boolean = {
      (c.isDigit && c.toInt - 48 < base) || (base > 10 && c.isLetter && c.toUpper < 55 + base)
    }

    //Only returns a value if the char is a valid digit in this base
    def toInt(c: Char, base: Int): Option[Int] = {
      if (c.isDigit && c.toInt - 48 < base) {
        Some(c.toString.toInt)
      } else if (isDigit(c, base)) {
        Some(c.toInt - 55)
      } else {
        None
      }
    }

    //Returns the string digit that contains that numerical value, if it is
    //a valid digit in this base
    def toString(value: Int, base: Int): Option[String] = {
      if (value >= base) {
        None
      } else if (value < 10) {
        Some(value.toString)
      } else {
        Some((value + 55).toChar.toString)
      }
    }


    //Returns the value of the last character of a number
    def last(n: String): Option[Int] = if (n.isEmpty) Some(0) else toInt(n.last, base) match {
      case None => None
      case Some(n: Int) => Some(n)
    }

    //Returns the 'next' string where the last digit is removed
    def next(n: String): String = if (n.isEmpty) "" else n.substring(0, n.length - 1)

    //Turns a number in base 10 into a specified base
    def convertFromDecimal(n: Int, base: Int): String = {
      if (n == 0) "" else convertFromDecimal(n / base, base) + toString(n % base, base)
    }

    //Does not work with bases above 10
    @Deprecated
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

  }

}