package com.nthportal.strawman.eql

import java.lang.Double.{doubleToLongBits, doubleToRawLongBits}
import java.lang.Float.{floatToIntBits, floatToRawIntBits}

trait Eql[T] extends Equiv[T] {
  def equal(x: T, y: T): Boolean

  def notEqual(x: T, y: T): Boolean = !equal(x, y)

  override final def equiv(x: T, y: T): Boolean = equal(x, y)
}

object Eql {
  @inline def apply[T: Eql]: Eql[T] = implicitly[Eql[T]]

  def by[T, S](f: T => S)(implicit eql: Eql[S]): Eql[T] =
    (x, y) => eql.equal(f(x), f(y))

  /* primitives */
  implicit object Unit extends Eql[Unit] {
    override def equal(x: Unit, y: Unit): Boolean = true
    override def notEqual(x: Unit, y: Unit): Boolean = false
  }

  implicit object Nothing extends Eql[Nothing] {
    override def equal(x: Nothing, y: Nothing): Boolean = throw new AssertionError("can't test equality of nothing")
  }

  trait ByteEql extends Eql[Byte] {
    override def equal(x: Byte, y: Byte): Boolean = x == y
    override def notEqual(x: Byte, y: Byte): Boolean = x != y
  }
  implicit object Byte extends ByteEql

  trait ShortEql extends Eql[Short] {
    override def equal(x: Short, y: Short): Boolean = x == y
    override def notEqual(x: Short, y: Short): Boolean = x != y
  }
  implicit object Short extends ShortEql

  trait IntEql extends Eql[Int] {
    override def equal(x: Int, y: Int): Boolean = x == y
    override def notEqual(x: Int, y: Int): Boolean = x != y
  }
  implicit object Int extends IntEql

  trait LongEql extends Eql[Long] {
    override def equal(x: Long, y: Long): Boolean = x == y
    override def notEqual(x: Long, y: Long): Boolean = x != y
  }
  implicit object Long extends LongEql

  trait CharEql extends Eql[Char] {
    override def equal(x: Char, y: Char): Boolean = x == y
    override def notEqual(x: Char, y: Char): Boolean = x != y
  }
  implicit object Char extends CharEql

  /* floating point, ugh */
  trait FloatEql extends Eql[Float] {
    override def equal(x: Float, y: Float): Boolean = floatToIntBits(x) == floatToIntBits(y)
    override def notEqual(x: Float, y: Float): Boolean = floatToIntBits(x) != floatToIntBits(y)
  }
  implicit object Float extends FloatEql

  trait IeeeFloatEql extends Eql[Float] {
    override def equal(x: Float, y: Float): Boolean = x == y
    override def notEqual(x: Float, y: Float): Boolean = x != y
  }
  implicit object IeeeFloat extends IeeeFloatEql

  trait FloatStrictEql extends Eql[Float] {
    override def equal(x: Float, y: Float): Boolean = floatToRawIntBits(x) == floatToRawIntBits(y)
    override def notEqual(x: Float, y: Float): Boolean = floatToRawIntBits(x) != floatToRawIntBits(y)
  }
  implicit object FloatStrict extends FloatStrictEql

  trait DoubleEql extends Eql[Double] {
    override def equal(x: Double, y: Double): Boolean = doubleToLongBits(x) == doubleToLongBits(y)
    override def notEqual(x: Double, y: Double): Boolean = doubleToLongBits(x) != doubleToLongBits(y)
  }
  implicit object Double extends DoubleEql

  trait IeeeDoubleEql extends Eql[Double] {
    override def equal(x: Double, y: Double): Boolean = x == y
    override def notEqual(x: Double, y: Double): Boolean = x != y
  }
  implicit object IeeeDouble extends IeeeDoubleEql

  trait DoubleStrictEql extends Eql[Double] {
    override def equal(x: Double, y: Double): Boolean = doubleToRawLongBits(x) == doubleToRawLongBits(y)
    override def notEqual(x: Double, y: Double): Boolean = doubleToRawLongBits(x) != doubleToRawLongBits(y)
  }
  implicit object DoubleStrict extends DoubleStrictEql

  /* core types */
  trait StringEql extends Eql[String] {
    override def equal(x: String, y: String): Boolean = x equals y
  }
  implicit object String extends StringEql

  implicit def optionEql[A](implicit eql: Eql[A]): Eql[Option[A]] =
    (x, y) =>
      if (x.isEmpty) y.isEmpty
      else y.isDefined && eql.equal(x.get, y.get)

  implicit def eitherEql[L, R](implicit eqlL: Eql[L], eqlR: Eql[R]): Eql[Either[L, R]] =
    (x, y) => (x, y) match {
      case (Left(xL), Left(yL)) => eqlL.equal(xL, yL)
      case (Right(xR), Right(yR)) => eqlR.equal(xR, yR)
      case _ => false
    }

  implicit def arrayEql[A](implicit eql: Eql[A]): Eql[Array[A]] =
    (x, y) =>
      if (x.length != y.length) false
      else x.indices forall { i => eql.equal(x(i), y(i)) }

  implicit def iterableEql[A](implicit eql: Eql[A]): Eql[Iterable[A]] =
    (x, y) => {
      val xIter = x.iterator
      val yIter = y.iterator

      var same = true
      while (xIter.hasNext && yIter.hasNext && same) {
        same = eql.equal(xIter.next(), yIter.next())
      }

      same && xIter.hasNext == yIter.hasNext
    }

  /* tuples */
  implicit def tuple2[T1, T2](implicit eql1: Eql[T1], eql2: Eql[T2]): Eql[(T1, T2)] =
    (x, y) => eql1.equal(x._1, y._1) && eql2.equal(x._2, y._2)

  implicit def tuple3[T1, T2, T3](implicit eql1: Eql[T1], eql2: Eql[T2], eql3: Eql[T3]): Eql[(T1, T2, T3)] =
    (x, y) =>
      eql1.equal(x._1, y._1) &&
        eql2.equal(x._2, y._2) &&
        eql3.equal(x._3, y._3)

  implicit def tuple4[T1, T2, T3, T4](implicit eql1: Eql[T1], eql2: Eql[T2], eql3: Eql[T3], eql4: Eql[T4]): Eql[(T1, T2, T3, T4)] =
    (x, y) =>
      eql1.equal(x._1, y._1) &&
        eql2.equal(x._2, y._2) &&
        eql3.equal(x._3, y._3) &&
        eql4.equal(x._4, y._4)

  implicit def tuple5[T1, T2, T3, T4, T5](implicit eql1: Eql[T1], eql2: Eql[T2], eql3: Eql[T3], eql4: Eql[T4], eql5: Eql[T5]): Eql[(T1, T2, T3, T4, T5)] =
    (x, y) =>
      eql1.equal(x._1, y._1) &&
        eql2.equal(x._2, y._2) &&
        eql3.equal(x._3, y._3) &&
        eql4.equal(x._4, y._4) &&
        eql5.equal(x._5, y._5)

  implicit def tuple6[T1, T2, T3, T4, T5, T6](implicit eql1: Eql[T1], eql2: Eql[T2], eql3: Eql[T3], eql4: Eql[T4], eql5: Eql[T5], eql6: Eql[T6]): Eql[(T1, T2, T3, T4, T5, T6)] =
    (x, y) =>
      eql1.equal(x._1, y._1) &&
        eql2.equal(x._2, y._2) &&
        eql3.equal(x._3, y._3) &&
        eql4.equal(x._4, y._4) &&
        eql5.equal(x._5, y._5) &&
        eql6.equal(x._6, y._6)

  implicit def tuple7[T1, T2, T3, T4, T5, T6, T7](implicit eql1: Eql[T1], eql2: Eql[T2], eql3: Eql[T3], eql4: Eql[T4], eql5: Eql[T5], eql6: Eql[T6], eql7: Eql[T7]): Eql[(T1, T2, T3, T4, T5, T6, T7)] =
    (x, y) =>
      eql1.equal(x._1, y._1) &&
        eql2.equal(x._2, y._2) &&
        eql3.equal(x._3, y._3) &&
        eql4.equal(x._4, y._4) &&
        eql5.equal(x._5, y._5) &&
        eql6.equal(x._6, y._6) &&
        eql7.equal(x._7, y._7)

  implicit def tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit eql1: Eql[T1], eql2: Eql[T2], eql3: Eql[T3], eql4: Eql[T4], eql5: Eql[T5], eql6: Eql[T6], eql7: Eql[T7], eql8: Eql[T8]): Eql[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    (x, y) =>
      eql1.equal(x._1, y._1) &&
        eql2.equal(x._2, y._2) &&
        eql3.equal(x._3, y._3) &&
        eql4.equal(x._4, y._4) &&
        eql5.equal(x._5, y._5) &&
        eql6.equal(x._6, y._6) &&
        eql7.equal(x._7, y._7) &&
        eql8.equal(x._8, y._8)

  implicit def tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit eql1: Eql[T1], eql2: Eql[T2], eql3: Eql[T3], eql4: Eql[T4], eql5: Eql[T5], eql6: Eql[T6], eql7: Eql[T7], eql8: Eql[T8], eql9: Eql[T9]): Eql[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    (x, y) =>
      eql1.equal(x._1, y._1) &&
        eql2.equal(x._2, y._2) &&
        eql3.equal(x._3, y._3) &&
        eql4.equal(x._4, y._4) &&
        eql5.equal(x._5, y._5) &&
        eql6.equal(x._6, y._6) &&
        eql7.equal(x._7, y._7) &&
        eql8.equal(x._8, y._8) &&
        eql9.equal(x._9, y._9)

  object Implicits {
    implicit def cooperative[T]: Eql[T] = _ == _
    implicit def universal[T <: AnyRef]: Eql[T] = (x, y) =>
      if (x eq null) y eq null else x equals y
    implicit def refIdentity[T <: AnyRef]: Eql[T] = _ eq _
  }

  object Conversions {
    implicit def fromEquiv[T](implicit equiv: Equiv[T]): Eql[T] = equiv.equiv _
  }
}
