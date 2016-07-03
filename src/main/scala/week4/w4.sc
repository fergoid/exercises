
import java.util.NoSuchElementException

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]  {
  def isEmpty = false;
  override def toString = "( " + tail + " - " + head + " )"
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains (x: Int): Boolean
  def union(other: IntSet): IntSet
}


object Empty extends IntSet {
  def contains(x: Int): Boolean=false
  def incl(x: Int): IntSet = new NonEmpty(x,  Empty, Empty)
  def union(other: IntSet) = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + "<" + elem + ">" + right + "}"
}

object test {
  val x: List[String] = Nil

}