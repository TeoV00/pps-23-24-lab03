package u02

import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil}
import u03.Sequences.Sequence.flatMap

object Modules extends App :

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n
    def extractCourses(p: Sequence[Person]): Sequence[String] =
      flatMap[Person, String](p) {
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      }

  println(Person.name(Person.Student("mario", 2015)))

  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  println(isStudent(Student("mario", 2015)))
