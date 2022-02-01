trait List [+A] {
  def size: Long
  def isEmpty: Boolean

  def head: A
  def last: A
  def headOpt: Option[A]
  def lastOpt: Option[A]
  def tail: List[A]
  def init: List[A]

  // should return index of given element
  def indexOf[B >: A](elem: B): Long

  // Returns first n elements of the list
  // if n > list size it will return the whole list
  def take (n: Int): List[A]

  // returns last n elements of the list
  def takeRight(n: Int): List[A]

  def reverse: List[A]

  def zip [B] (that: List[B]): List[(A, B)]
  def zipWithIndex: List[(A, Long)]

  def append[B >: A](that: List[B]): List[B]

  // appends element to the beginning of list
  def cons[B >: A](a: B): List[B]

  // High order functions
  // Recursively implement the following methods:
  def map [B] (f: A => B): List[B]
  def filter (predicate: A => Boolean): List[A]
  def flatMap[B] (f: A => List[B]): List[B]

  def count(predicate: A => Boolean): Int
  def find (predicate: A => Boolean): Option[A]
  def exists(predicate: A => Boolean): Boolean

  // Partial function
  // creates a new collection by its partial application to all elements
  // of given list where this function is defined
  def collect [B](pfun: PartialFunction[A, B]): List[B]

  // Currying
  def foldLeft [B] (z: B) (operator: (B, A) => B): B
  def foldRight [B] (z: B) (operator: (A, B) => B): B
}


object List {
  def apply[A] (items: A*): List[A] = ???
//    if (items.isEmpty) Nil()
//    else apply(items.tail).cons(items.head)

  def fill [A] (value: A)(size: Int): List[A] =
    if(size <= 0) Nil()
    else fill(value)(size - 1).cons(value)

  def empty [A]: List[A] = Nil()

  def flatten[A](list: List[List[A]]): List[A] =
    if (list.isEmpty) Nil()
    else list.head.append(flatten(list.tail))
}

case class Cons[+A](head: A, tail: List[A], size: Long) extends List[A] {

  def isEmpty: Boolean = false

  def last: A = if (tail.isEmpty) head else tail.last

  def headOpt: Option[A] = Some(head)

  def lastOpt: Option[A] = Some(last)

  def init: List[A] = if (tail.isEmpty) tail else tail.init.cons(head)

  def indexOf[B >: A](elem: B): Long =
    if (elem == head) 0 else
      if(tail.indexOf(elem) < 0) -1 else 1 + tail.indexOf(elem)

  def take(n: Int): List[A] = if (n <= 0) Nil() else tail.take(n - 1).cons(head)

  def takeRight(n: Int): List[A] = this.reverse.take(n)

  def reverse: List[A] = foldLeft(List())((acc, elem) => acc.cons(elem))

  def zip[B](that: List[B]): List[(A, B)] =
    if(that.isEmpty) Nil()
    else tail.zip(that.tail).cons(head, that.head)

  def zipWithIndex: List[(A, Long)] = {
    def iterate(counter: Long, list: List[A]): List[(A, Long)] = list match {
      case Cons(head, tail, size) if tail.isEmpty => Nil().cons(head, counter)
      case Cons(head, tail, size) => iterate(counter + 1, tail).cons(head, counter)
    }
    iterate(0, this)
  }

  def append[B >: A](that: List[B]): List[B] = tail.append(that).cons(head)

  def cons[B >: A](a: B): List[B] = Cons(a, this, size + 1)

  def map[B](f: A => B): List[B] = tail.map(f).cons(f(head))

  def flatMap[B] (f: A => List[B]): List[B] = List.flatten(map(f))

  def filter(predicate: A => Boolean): List[A] =
    if(predicate(head)) tail.filter(predicate).cons(head)
    else tail.filter(predicate)

  def count(predicate: A => Boolean): Int =
    if(predicate(head)) 1 + tail.count(predicate)
    else tail.count(predicate)

  def find(predicate: A => Boolean): Option[A] =
    if(predicate(head)) Some(head) else tail.find(predicate)

  def exists(predicate: A => Boolean): Boolean =
    predicate(head) || tail.exists(predicate)

  def collect[B](pfun: PartialFunction[A, B]): List[B] =
    if (pfun.isDefinedAt(head)) tail.collect(pfun).cons(pfun(head))
    else tail.collect(pfun)

  def foldLeft[B](z: B)(operator: (B, A) => B): B =
    tail.foldLeft(operator(z, head))(operator)

  def foldRight[B](z: B)(operator: (A, B) => B): B =
    operator(head, tail.foldRight(z)(operator))

}

 case class Nil[+A]() extends List[A] {
   def size: Long = 0

   def isEmpty: Boolean = true

   def head: A = throw new Error("EmptyList")

   def last: A = throw new Error("EmptyList")

   def headOpt: Option[A] = Option.empty

   def lastOpt: Option[A] = Option.empty

   def tail: List[A] = throw new Error("EmptyList")

   def init: List[A] = throw new Error("EmptyList")

   def indexOf[B >: A](elem: B): Long = -1

   def take(n: Int): List[A] = this

   def takeRight(n: Int): List[A] = this

   def reverse: List[A] = this

   def zip[B](that: List[B]): List[(A, B)] = Nil()

   def zipWithIndex: List[(A, Long)] = Nil()

   def append[B >: A](that: List[B]): List[B] = that

   def cons[B >: A](a: B): List[B] = Cons(a, this, 1)

   def map[B](f: A => B): List[B] = Nil()

   def flatMap[B] (f: A => List[B]): List[B] = Nil()

   def filter(predicate: A => Boolean): List[A] = this

   def count(predicate: A => Boolean): Int = 0

   def find(predicate: A => Boolean): Option[A] = Option.empty

   def exists(predicate: A => Boolean): Boolean = false

   def collect[B](pfun: PartialFunction[A, B]): List[B] = Nil()

   def foldLeft[B](z: B)(operator: (B, A) => B): B = z

   def foldRight[B](z: B)(operator: (A, B) => B): B = z
 }