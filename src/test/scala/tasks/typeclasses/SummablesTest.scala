package tasks.typeclasses

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.*
import Sequence.*
import u04lab.Ex4Summables.sumAllInt
import u04lab.Ex4Summables.sumAll

class SummablesTest:

  val sumint = Cons(10, Cons(20, Cons(30, Nil())))
  val sumdouble = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
  val sumstring = Cons("10", Cons("20", Cons("30", Nil())))

  @Test def testSumAllInt(): Unit =
    assertEquals(60, sumAllInt(sumint))

  @Test def testSumAllWithInt() =
    assertEquals(60, sumAll(sumint))

  @Test def testSumAllWithDouble() =
    assertEquals(60.0, sumAll(sumdouble), 0)

  @Test def testSumAllWithString() =
    assertEquals("102030", sumAll(sumstring))