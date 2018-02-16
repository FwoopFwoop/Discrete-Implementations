package main.scala

object Exponentiation extends App {
  //Calculates a^p (mod n)
  def fastModular(a: Int, p: Int, n: Int): Int = {
    Conversion.binaryPowers(p).map(b =>
      if (b == 0) {
        1
      } else {
        fastModular(a, fastModular(2, b, n), n)
      }).reduce((a, b) => a.%(n).*(b.%(n)).%(n))
  }

  print(fastModular(5, 96, 97))
}
