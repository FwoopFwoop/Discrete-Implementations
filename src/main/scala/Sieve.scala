package main.scala

object Sieve extends App{
  def primesUntil(limit: Int): Vector[Int] = {
    def sieveAcc(range:Vector[Int], primes:Vector[Int]): Vector[Int] ={
      if(range.isEmpty){
        primes
      } else {
        sieveAcc(range.tail.filter(n => n % range.head != 0), primes :+ range.head)
      }
    }
    sieveAcc((2 to limit).toVector, Vector())
  }

  primesUntil(100).foreach(println)
}
