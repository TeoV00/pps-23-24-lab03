package u03.lab03tasks

import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u02.Modules.Person
import u02.Modules.Person.*

import scala.annotation.tailrec

class Lab03Tasks:

  // Lab 03
  // 1.a
  def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
    case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
    case _ => Nil()

  // 1.b
  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(fh, ft), Cons(sh, st)) => Cons((fh, sh), zip(ft, st))
    case (Nil(), _) | (_, Nil()) | (Nil(), Nil()) => Nil()

  // 1.c
  def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
    case (Nil(), _) => l2
    case (Cons(h, Nil()), _) => Cons(h, l2)
    case (Cons(h, l1b), l2) => Cons(h, concat(l1b, l2))

  // 1.c
  def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  // 1.d
  def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(v => Cons(mapper(v), Nil()))

  // 1.d
  def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
    case Cons(h, t) if pred(h) => flatMap(Cons(h, filter(t)(pred)))(v => Cons(v, Nil()))
    case Cons(_, t) => filter(t)(pred)
    case Nil() => Nil()

  // 2
  def min(l: Sequence[Int]): Optional[Int] =
    @tailrec
    def minRec(l: Sequence[Int], m: Optional[Int]): Optional[Int] = l match
      case Cons(h, t) if h <= Optional.orElse(m, h) => minRec(t, Optional.Just(h))
      case Cons(_, t) => minRec(t, m)
      case Nil() => m
    minRec(l, Optional.Empty())

  // 3
  def extractCourses(p: Sequence[Person]): Sequence[String] =
    flatMap[Person, String](p) {
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    }

