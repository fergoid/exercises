object mapreduce {
  def mapReduce(f: Double => Double, combine: (Double, Double) => Double, zero: Double)(a: Double, b: Double): Double =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def product(f: Double => Double)(a: Double, b: Double): Double = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def fact(n: Double) = product(x => x)(1, n)  // x=>x allows us to change how n is passed (e.g. value, square, cube etc.)

  fact(10)

}