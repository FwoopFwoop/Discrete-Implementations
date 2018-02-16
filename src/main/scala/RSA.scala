package main.scala

object RSA extends App {
  def factorial(n:Int):Int = if (n<=0) 1 else n*factorial(n-1)

  def eX(x:Int):Double = (0 to 100).map((n)=>(math.pow(x,n)/factorial(n))).sum
}


