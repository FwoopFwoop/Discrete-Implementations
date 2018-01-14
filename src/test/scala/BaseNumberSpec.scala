import main.scala.BaseNumber
import org.scalatest.{FlatSpec, Matchers}

class BaseNumberSpec extends FlatSpec with Matchers {
  "A BaseNumber" should "know if it is valid" in {
    val n = new BaseNumber("192", 2)
    val m = new BaseNumber("FFA20", 16)
    val o = new BaseNumber("100", 50)

    n.isValidNumber() should be
    false
    m.isValidNumber() should be
    true
    o.isValidNumber() should be
    false
  }

  it should "be able to produce its decimal value if it is valid" in {
    val a = new BaseNumber("11", 2)
    val b = new BaseNumber("FF", 16)
    val c = new BaseNumber("420", 10)
    val d = new BaseNumber("A", 2)

    a.toDecimal() should contain
    3
    b.toDecimal() should contain
    255
    c.toDecimal() should contain
    420
    d.toDecimal().isEmpty should be
    true
  }

  it should "be able to produce its string representation if it is valid" in {
    val a = new BaseNumber("11", 2)
    val b = new BaseNumber("FF", 16)
    val c = new BaseNumber("420", 10)
    val d = new BaseNumber("A", 2)

    a.toString should contain "11"
    b.toString should contain "FF"
    c.toString should contain "420"
    d.toString.isEmpty should be true
  }

  it should "have functioning addition where the result is in the base of the first number" in{
    val a = new BaseNumber("10", 2)
    val b = new BaseNumber("2", 16)

    a + b should contain new BaseNumber("100", 2)

    val m = new BaseNumber("100", 2)
    val n = new BaseNumber("watermelon", 3)

    (m + n).isEmpty should be true
  }

  it should "have functioning subtraction where the result in the base of the first number" in{
    val a = new BaseNumber("101", 2)
    val b = new BaseNumber("10", 3)

    a - b should contain new BaseNumber("10", 2)
  }
}