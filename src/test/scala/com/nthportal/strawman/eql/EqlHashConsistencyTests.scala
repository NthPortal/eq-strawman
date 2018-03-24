package com.nthportal.strawman.eql

import com.nthportal.strawman.eql.Hasher.IntHasher
import com.nthportal.strawman.eql.Values._
import utest._

import scala.reflect.ClassTag

object EqlHashConsistencyTests extends TestSuite {
  private[this] val badDoubles = Seq(dNaN1, dNaN2, 0.0, -0.0)
  private[this] val badFloats = Seq(fNaN1, fNaN2, 0.0f, -0.0f)

  val tests = Tests {
    def checkAll[A: Eql : Hash](values: Iterable[A]): Unit =
      for {
        x <- values
        y <- values
        if x === y
      } assert(IntHasher.hash(x) == IntHasher.hash(y))

    'primitives - {
      'Unit - checkAll(Seq(()))
      'Byte - checkAll(Seq(Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue))
      'Short - checkAll(Seq(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue))
      'Int - checkAll(ints)
      'Long - checkAll(Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue))
      'Char - checkAll(Seq(0.toChar, 'x', ' ', '‌' /*ZWNJ*/ , '\t', '\r', '\n'))
      'Float - {
        'basics - checkAll(floats)(Eql.Float, Hash.Float)
        "NaN+zero" - {
          'default - checkAll(badFloats)(Eql.Float, Hash.Float)
          'IEEE - checkAll(badFloats)(Eql.IeeeFloat, Hash.IeeeFloat)
          'strict - checkAll(badFloats)(Eql.FloatStrict, Hash.FloatStrict)
        }
      }
      'Double - {
        'basics - checkAll(doubles)(Eql.Double, Hash.Double)
        "NaN+zero" - {
          'default - checkAll(badDoubles)(Eql.Double, Hash.Double)
          'IEEE - checkAll(badDoubles)(Eql.IeeeDouble, Hash.IeeeDouble)
          'strict - checkAll(badDoubles)(Eql.DoubleStrict, Hash.DoubleStrict)
        }
      }
    }
    'core - {
      'Option - {
        def some[A](value: A): Option[A] = Some(value)

        checkAll(ints map some)
        checkAll(strings map some)
      }
      'Either - {
        def left[L](value: L): Either[L, Void] = Left(value)
        def right[R](value: R): Either[Void, R] = Right(value)

        checkAll(ints map left)
        checkAll(ints map right)
        checkAll((strings map left) ++ (strings map right))
      }
      'Array - {
        def check[A: Eql : Hash : ClassTag](values: Seq[A]): Unit = {
          val arrays = for {
            size <- 1 to 2
            v1 <- values
            v2 <- values
          } yield Array(v1, v2) take size

          checkAll(arrays)
        }

        check(ints)
        check(strings)
      }
      'Iterable - {
        def check[A: Eql : Hash](values: Seq[A]): Unit = {
          val iterables = for {
            size <- 1 to 2
            v1 <- values
            v2 <- values
          } yield IndexedSeq(v1, v2) take size: Iterable[A]

          checkAll(iterables)
        }

        check(ints)
        check(strings)
      }
    }
    'tuples - {
      'Tuple2 - {
        def check[A: Eql : Hash, B: Eql : Hash](as: Seq[A], bs: Seq[B]): Unit = {
          val tuples = for {
            a <- as
            b <- bs
          } yield (a, b)

          checkAll(tuples)
        }

        check(ints, strings)
      }
      'Tuple3 - {
        def check[A: Eql : Hash, B: Eql : Hash, C: Eql : Hash](as: Seq[A], bs: Seq[B], cs: Seq[C]): Unit = {
          val tuples = for {
            a <- as
            b <- bs
            c <- cs
          } yield (a, b, c)

          checkAll(tuples)
        }

        check(ints, strings, strings)
      }
    }

  }
}
