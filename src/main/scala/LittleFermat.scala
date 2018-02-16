package main.scala

object LittleFermat extends App {
  def isPrime(p: Int): Boolean = {
    def loop(uncertainty: Double): Boolean = {
      if (uncertainty < .1) true

      else {
        val a = scala.util.Random.nextInt(p)

        if (a == 0) loop(uncertainty)

        else if (Exponentiation.fastModular(a, p - 1, p) == 1) {
          loop(uncertainty * .75)
        } else false
      }
    }
    loop(100)
  }

  print(isPrime(97).toString)
}