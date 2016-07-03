

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }

}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => show(l) + " + " + show(r)
}

show(Number(1))
show(Sum(Number(1), Number(2)))

val fruit = List("apples", "oranges", "pears")
val colour = List("green", "orange", "yellow")
fruit.zip(colour).count(p => p._1 == p._2)

fruit.head
fruit.tail.head
val y = "x" :: Nil

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x<=y) x::xs else  y::insert(x, ys)
}

val x = isort(List(8,9,5,4,7))

val z = List(1,2,3)
z.foldRight("x")((b, a) => b + a)
z.foldLeft(0)((b, a) => b + a)
z.foldLeft(1)((b,a) => b*a)
z.foldLeft(List[Int]())((b,a) => a :: b)

var shortList = 1 to 10 toList
var longList = 1 to 325000 toList

shortList.foldLeft(1) ((x, y) => "x")


