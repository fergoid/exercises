package week3

class Rational (x: Int, y: Int) {
  //require(y > 0, "denominator must be greater than zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x,y)
  val numer = x / g
  val denom = y / g

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom ,
      denom * that.denom)


  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this.less(that)) that else this

  override def toString() = numer + "/" + denom
}
