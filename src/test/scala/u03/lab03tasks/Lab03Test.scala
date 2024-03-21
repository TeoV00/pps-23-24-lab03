package u03.lab03tasks

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Sequences.Sequence.*
import u03.Streams.Stream.takeWhile

class Task3:
  import u02.Modules.Person.*
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
    assertEquals(16, lst.foldLeft(0.0)(_ + _), 0.5)
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(0, foldLeft(Nil[Int]())(0)(_ + _))
    assertEquals("3715", lst.foldLeft("")(_ + _))

class Task6:
  import u03.Streams.*
  @Test def testTakeWhile(): Unit =
    val s = Stream.iterate(0)(_ + 1)
    assertEquals(Cons(0, Cons(1, Cons(2, Nil()))), Stream.toList(takeWhile(s)(_ < 3)))

  @Test def testFill(): Unit =
    import u03.Streams.Stream.fill
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(fill(3)("a")))