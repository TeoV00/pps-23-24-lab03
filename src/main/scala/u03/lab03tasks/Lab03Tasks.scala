package u03.lab03tasks

import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{flatMap, *}
import u02.Modules.Person
import u02.Modules.Person.*

import scala.annotation.tailrec

object Lab03Tasks:
  // Lab 03

  // Task 1
  extension [A](s: Sequence[A])
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
    def foldLeft(i: A)(o: (A, A) => A): A = s match
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


