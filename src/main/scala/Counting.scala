package main.scala

object Counting {
  def permutation(n: Int, r: Int): Int = {
    if (n == 0 || r == 0)
      1
    else
      n * permutation(n - 1, r)
  }
}