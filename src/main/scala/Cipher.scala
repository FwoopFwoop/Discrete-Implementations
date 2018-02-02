package main.scala

object Cipher extends App{

  def linearCipher(m: String, k: Int, c: Int) =
    m.map(c => (((((c.toUpper - 65) * k) + c) % 26) + 65).toChar.toString).reduce((s1,s2)=> s1 + s2)

  print(linearCipher("HELLO", 3, 16))
}

