def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  if (as.length < 2) true
  else gt(as(0), as(1)) && isSorted(as.tail, gt)
}

println(isSorted(Array[Int](1, 2, 3, 4), (i1: Int, i2: Int) => i1 < i2))
println(isSorted(Array[Int](1, 3, 2, 4), (i1: Int, i2: Int) => i1 < i2))