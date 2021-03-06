package main.scala

object Euclidean extends App {
  def classic(a: Int, b: Int): Int = gcm(a, b, (a, b) => a - b)

  def modern(a: Int, b: Int): Int = gcm(a, b, (a, b) => a % b)

  private def gcm(a: Int, b: Int, op: (Int, Int) => Int): Int = {
    val p = if (a > b) (a, b) else (b, a)

    if (p._1 % p._2 == 0) p._2 else classic(op(p._1, p._2), p._2)
  }

  //Returns the tuple (x,y,d)
  def extended(a: Int, b: Int): (Int, Int, Int) = {
    val p = if (a > b) (a, b) else (b, a)

    if (p._1 % p._2 == 0) (0, 1, p._2) else {
      val last = extended(p._2, p._1 % p._2)

      (last._2, last._1 - ((p._1/p._2) * last._2), last._3)
    }
  }

  print(extended(5678, 1234))
}