def abs(n: Int): Int =
  if (n < 0) -n
  else n

def formatAbs(x: Int) = {
  val msg = "The absolute value of %d is %d"
  msg.format(x, abs(x))
}

println(formatAbs(-42))