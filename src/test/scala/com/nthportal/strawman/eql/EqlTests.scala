package com.nthportal.strawman.eql

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}

import utest._
import com.nthportal.strawman.eql.Seqs._

import scala.reflect.ClassTag

object EqlTests extends TestSuite {
  private[this] val dNaN1 = Double.NaN
  private[this] val dNaN2 = longBitsToDouble(doubleToRawLongBits(Double.NaN) ^ Long.MinValue)
  private[this] val fNaN1 = Float.NaN
  private[this] val fNaN2 = intBitsToFloat(floatToRawIntBits(Float.NaN) ^ Int.MinValue)

  val tests = Tests {
    'primitives - {
      def checkAll[A: Eql](values: A*): Unit =
        for (x <- values; y <- values) (x == y) ==> (x === y)

      'Unit - checkAll(())
      'Byte - checkAll(Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue)
      'Short - checkAll(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue)
      'Int - checkAll(ints: _*)
      'Long - checkAll(Long.MinValue, -1L, 0L, 1L, Long.MaxValue)
      'Char - checkAll(0.toChar, 'x', ' ', '‌'/*ZWNJ*/, '\t', '\r', '\n')
      'Float - {
        'basics - checkAll(floats: _*)(Eql.Float)
        "NaN+zero" - {
          'default - {
            import Eql.Float
            assert(fNaN1 === fNaN1)
            assert(fNaN1 === fNaN2)
            assert(0.0f =!= -0.0f)
          }
          'IEEE - {
            import Eql.IeeeFloat
            assert(fNaN1 =!= fNaN1)
            assert(fNaN1 =!= fNaN2)
            assert(0.0f === -0.0f)
          }
          'strict - {
            import Eql.FloatStrict
            assert(fNaN1 === fNaN1)
            assert(fNaN1 =!= fNaN2)
            assert(0.0f =!= -0.0f)
          }
        }
      }
      'Double - {
        'basics - checkAll(doubles: _*)(Eql.Double)
        "NaN+zero" - {
          'default - {
            import Eql.Double
            assert(dNaN1 === dNaN1)
            assert(dNaN1 === dNaN2)
            assert(0.0 =!= -0.0)
          }
          'IEEE - {
            import Eql.IeeeDouble
            assert(dNaN1 =!= dNaN1)
            assert(dNaN1 =!= dNaN2)
            assert(0.0 === -0.0)
          }
          'strict - {
            import Eql.DoubleStrict
            assert(dNaN1 === dNaN1)
            assert(dNaN1 =!= dNaN2)
            assert(0.0 =!= -0.0)
          }
        }
      }
    }
    'core - {
      import Eql.Double
      'Option - {
        def some[A](value: A): Option[A] = Some(value)
        def checkAll[A: Eql](values: Iterable[A]): Unit =
          for (x <- values; y <- values) (x === y) ==> (some(x) === some(y))

        checkAll(ints)
        checkAll(doubles)
        checkAll(strings)
      }
      'Either - {
        def left[L](value: L): Either[L, L] = Left(value)
        def right[R](value: R): Either[R, R] = Right(value)
        def checkAll[A: Eql](values: Iterable[A]): Unit = {
          for (x <- values; y <- values) {
            (x === y) ==> (left(x) === left(y))
            (x === y) ==> (right(x) === right(y))
          }
          for (x <- values) assert(left(x) =!= right(x))
        }

        checkAll(ints)
        checkAll(doubles)
        checkAll(strings)
      }
      'Array - {
        def sameArrays[A: Eql](x: Array[A], y: Array[A]): Boolean = {
          x.length == y.length &&
            (x.indices forall {i => x(i) === y(i)})
        }
        def checkAll[A : Eql : ClassTag](values: Iterable[A]): Unit = {
          val arrays = for {
            size <- 1 to 2
            v1 <- values
            v2 <- values
          } yield Array(v1, v2) take size

          for (x <- arrays; y <- arrays) sameArrays(x, y) ==> (x === y)
        }

        checkAll(ints)
        checkAll(doubles)
        checkAll(strings)
      }
      'Iterable - {
        def sameIterables[A: Eql](x: Iterable[A], y: Iterable[A]): Boolean = {
          x.size == y.size &&
            (0 until x.size forall {i => x.toSeq(i) === y.toSeq(i)})
        }
        def checkAll[A: Eql](values: Iterable[A]): Unit = {
          val arrays = for {
            size <- 1 to 2
            v1 <- values
            v2 <- values
          } yield IndexedSeq(v1, v2) take size: Iterable[A]

          for (x <- arrays; y <- arrays) sameIterables(x, y) ==> (x === y)
        }

        checkAll(ints)
        checkAll(doubles)
        checkAll(strings)
      }
    }
    'tuples - {
      import Eql.Double
      'Tuple2 - {
        def checkAll[A: Eql, B: Eql](as: Iterable[A], bs: Iterable[B]): Unit = {
          val tuples = for {
            a <- as
            b <- bs
          } yield (a, b)

          for (x <- tuples; y <- tuples) {
            (x._1 === y._1 &&
              x._2 === y._2) ==> (x === y)
          }
        }

        checkAll(ints, strings)
      }
      'Tuple3 - {
        def checkAll[A: Eql, B: Eql, C: Eql](as: Iterable[A], bs: Iterable[B], cs: Iterable[C]): Unit = {
          val tuples = for {
            a <- as
            b <- bs
            c <- cs
          } yield (a, b, c)

          for (x <- tuples; y <- tuples) {
            (x._1 === y._1 &&
              x._2 === y._2 &&
              x._3 === y._3) ==> (x === y)
          }
        }

        checkAll(ints, doubles, strings)
      }
    }
  }
}