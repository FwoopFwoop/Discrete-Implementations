package main.scala

object Conversion extends App {
  def fromDecimal(n: Int, base: Int): String = {
    if (n == 0) "" else fromDecimal(n / base, base) + (n % base).toString
  }

  def binaryPowers(n: Int): List[Int] = {
    def loop(p: Int, n: Int): List[Int] = {
      if (n == 0) {
        List()
      } else if (n % 2 == 1) {
        p +: loop(p + 1, n / 2)
      } else {
        loop(p + 1, n / 2)
      }
    }

    loop(0, n)
  }

  print(binaryPowers(7))
}
