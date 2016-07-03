def a: List[Char] = List('h','e','l','l','o','h')
a.toString()
var b = a.groupBy(x => x).map(x => (x._1, x._2.size)).toList
b.sortWith(_._2 < _._2)


(1 to 10).scanLeft(0)(_ + _)

(0 /: List(1,2,3).reverse)(_ + _)

val prices = List(5.0, 20.0, 9.95)
val quantities = List(10, 2, 1)
prices zip quantities

(prices zip quantities) map { p => p._1 * p._2 }

"Mississipi".zipWithIndex
"Mississipi".zipWithIndex.max
"Mississipi".zipWithIndex.max._2

def numsFrom(n: BigInt): Stream[BigInt] = n #:: numsFrom(n + 1)
val tenOrMore = numsFrom(10)
tenOrMore.tail.tail

