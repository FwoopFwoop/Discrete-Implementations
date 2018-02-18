package main.scala

object Exponentiation extends App {
  //Calculates a^p (mod n)
  def fastModular(a: Int, p: Int, n: Int): Int = {
    def repeatSquare(b: Int): Int={
      if(b == 0){
        1
      }
      else if(b == 1){
        a
      }else{
        (repeatSquare(b-1) * repeatSquare(b-1)) % n;
      }
    }
    Conversion.binaryPowers(p).map(repeatSquare).reduce((a, b) => a.%(n).*(b.%(n)).%(n))
  }

  print(fastModular(103, 9, 187))
}
