package main.scala

object Cipher extends App{

  def linearCipher(m: String, k: Int, b: Int) =
    m.map(c => ((((c.toUpper.toInt - 65) * k + b) % 26) + 65).toChar)

  print(linearCipher("HELLO", 3, 16))
}

