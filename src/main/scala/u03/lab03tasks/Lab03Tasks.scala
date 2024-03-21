package u03.lab03tasks

import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{flatMap, *}
import u02.Modules.Person
import u02.Modules.Person.*
import u03.Streams.Stream

import scala.annotation.tailrec

object Lab03Tasks:
  // Lab 03
  extension [A](s: Sequence[A])
    // Task 1
    // 1.a
    def take(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
      case _ => Nil()

    // 1.b
    def zip[B](second: Sequence[B]): Sequence[(A, B)] = (s, second) match
      case (Cons(fh, ft), Cons(sh, st)) => Cons((fh, sh), ft.zip(st))
      case (Nil(), _) | (_, Nil()) | (Nil(), Nil()) => Nil()

    // 1.c
    def concat(l2: Sequence[A]): Sequence[A] = s match
      case Nil() => l2
      case Cons(h, Nil()) => Cons(h, l2)
      case Cons(h, l1b) => Cons(h, l1b.concat(l2))

    // 1.c
    def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
      case _ => Nil()

    // 1.d
    def map[B](mapper: A => B): Sequence[B] = s.flatMap(v => Cons(mapper(v), Nil()))

    // 1.d
    def filter(pred: A => Boolean): Sequence[A] =
      s.flatMap:
        case e if pred(e) => Cons(e, Nil())
        case _ => Nil()

    // Task 4
    @tailrec
    def foldLeft[B](i: B)(o: (B, A) => B): B = s match
      case Cons(h, t) => t.foldLeft(o(i, h))(o)
      case Nil() => i

  // Task 2
  def min(l: Sequence[Int]): Optional[Int] =
    @tailrec
    def minRec(l: Sequence[Int], m: Optional[Int]): Optional[Int] = l match
      case Cons(h, t) if h <= Optional.orElse(m, h) => minRec(t, Optional.Just(h))
      case Cons(_, t) => minRec(t, m)
      case Nil() => m
    minRec(l, Optional.Empty())

  // Task 3
  def extractCourses(p: Sequence[Person]): Sequence[String] =
    p.flatMap:
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()

  // Task 6
  /*
  import u03.Streams.Stream
  def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
    case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
    case _ => Empty()

  // Task 7
  def fill[A](amount: Int)(elem: A): Stream[A] = amount match
    case a if a > 0 => cons(elem, fill(amount - 1)(elem))
    case _ => empty()
*/

  // Task 8
  def pellRec(i: Int): Int = i match
    case 0 | 1 => i
    case n => 2 * pellRec(n - 1) + pellRec(n - 2)

  def pellStream(n: Int): Stream[Int] =
    val s = Stream.take(Stream.iterate(-1)(_ + 1))(n)
    Stream.map[Int, Int](s)(v =>
      val p = pellRec(v + 1)
      println(p)
      p
    )
    /*
    Stream.take(Stream.iterate(0)(v =>
      val p = pellRec(v + 1)
      println(p)
      p
    ))(n)
     */