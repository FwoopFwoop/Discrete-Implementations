def ohw10(n:Int, sol: List[Int]): List[Int] ={
  if(sol.size == 4) sol
  else if(n.*(3).+(5).%(n) == 0) ohw10(n + 1, n+:sol)
  else ohw10(n+1, sol)
}

ohw10(1, List())