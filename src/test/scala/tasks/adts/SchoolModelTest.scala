package tasks.adts

import org.junit.*
import org.junit.Assert.*
import u03.extensionmethods.Sequences.Sequence.*

class SchoolModelTest:

  import tasks.adts.SchoolModel.BasicSchoolModule.*
  val school: School = emptySchool
  val john: Teacher = teacher("John")
  val pablo: Teacher = teacher("Pablo")
  val math: Course = course("Math")
  val italian: Course = course("Italian")
  val school2: School = school.setTeacherToCourse(john, math)
  val school3: School = school2.setTeacherToCourse(john, italian).setTeacherToCourse(pablo, italian)

  @Test def testTeacher() =
    assertEquals(cons("John", Nil()), school2.teachers())
    assertEquals(cons("John", cons("Pablo", Nil())), school3.teachers())
    assertEquals(nil(), school.teachers())

  @Test def testCourse() =
    assertEquals(cons("Math", Nil()), school2.courses())
    assertEquals(cons("Math", cons("Italian", Nil())), school3.courses())
    assertEquals(nil(), school.courses())

  @Test def testHasCourse() =
    assertFalse(school.hasCourse("Math"))
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school2.hasCourse("Italian"))
    assertFalse(school2.hasCourse("English"))
    assertTrue(school3.hasCourse("Math"))
    assertTrue(school3.hasCourse("Italian"))

  @Test def testHasTeacher() =
    assertFalse(school.hasTeacher("John"))
    assertTrue(school2.hasTeacher("John"))
    assertTrue(school3.hasTeacher("John"))
    assertTrue(school3.hasTeacher("Pablo"))

  @Test def testCoursesOfATeacher() =
    assertEquals(cons("Math", Nil()), school2.coursesOfATeacher(john))
    assertEquals(cons("Math", cons("Italian", Nil())), school3.coursesOfATeacher(john))
    assertEquals(cons("Italian", Nil()), school3.coursesOfATeacher(pablo))
    assertEquals(nil(), school.coursesOfATeacher(john))