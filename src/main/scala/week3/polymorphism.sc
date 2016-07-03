
import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]  {
  def isEmpty = false;
  override def toString = "( " + tail + " - " + head + " )"
}

class Nil[T] extends List[T] {
  def isEmpty = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

def a = singleton[Int](1)
a.toString
def b = singleton[Boolean](true)
b.toString

def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
      else  nth(n - 1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))


object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T]() = new Nil
}

val x: List(1,2)

