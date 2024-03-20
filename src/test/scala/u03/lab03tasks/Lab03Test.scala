package u03.lab03tasks

import org.junit.Assert.assertEquals
import org.junit.Test
import u02.Modules.Person.*
import u03.Sequences.Sequence.*

class Task3:
  @Test def testRetrieveAllTeacherCourses(): Unit =
    val persons = Cons(Teacher("n1", "c1"), Cons(Student("s1", 1980), Cons(Teacher("n2", "c2"), Nil())))
    assertEquals(Cons("c1", Nil()), extractCourses(Cons(Teacher("name", "c1"), Nil())))
    assertEquals(Cons("c1", Cons("c2", Nil())), extractCourses(persons))
    assertEquals(Nil(), extractCourses(Cons(Student("s1", 2000), Nil())))
    assertEquals(Nil(), extractCourses(Nil()))

class Task4:
  import u03.lab03tasks.Lab03Tasks.foldLeft
  @Test def testFoldLeftInt(): Unit =
    val lst = Cons(3, Cons(7,Cons(1, Cons(5, Nil()))))
    assertEquals(16, lst.foldLeft(0)(_ + _))
    assertEquals(-16, foldLeft[Int](lst)(0)(_ - _))
    assertEquals(0, foldLeft[Int](Nil())(0)(_ + _))
