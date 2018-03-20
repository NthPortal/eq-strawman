package com.nthportal.strawman.eql

import java.nio.charset.StandardCharsets
import java.{lang => jl}

trait Hash[T] {
  def hash(value: T, hasher: Hasher[_]): Unit
}

object Hash {
  @inline def apply[T: Hash]: Hash[T] = implicitly[Hash[T]]

  /* primitives */
  trait UnitHash extends Hash[Unit] {
    override def hash(value: Unit, hasher: Hasher[_]): Unit = hasher += 0
  }
  implicit object Unit extends UnitHash

  trait ByteHash extends Hash[Byte] {
    override def hash(value: Byte, hasher: Hasher[_]): Unit = hasher += value
  }
  implicit object Byte extends ByteHash

  trait ShortHash extends Hash[Short] {
    override def hash(value: Short, hasher: Hasher[_]): Unit = hasher += value.toInt
  }
  implicit object Short extends ShortHash

  trait IntHash extends Hash[Int] {
    override def hash(value: Int, hasher: Hasher[_]): Unit = hasher += value
  }
  implicit object Int extends IntHash

  trait LongHash extends Hash[Long] {
    override def hash(value: Long, hasher: Hasher[_]): Unit = hasher += value
  }
  implicit object Long extends LongHash

  trait CharHash extends Hash[Char] {
    override def hash(value: Char, hasher: Hasher[_]): Unit = hasher += value.toInt
  }
  implicit object Char extends CharHash

  /* floating point, ugh */
  trait FloatHash extends Hash[Float] {
    override def hash(value: Float, hasher: Hasher[_]): Unit = hasher += jl.Float.hashCode(value)
  }
  implicit object Float extends FloatHash

  trait IeeeFloatHash extends Hash[Float] {
    override def hash(value: Float, hasher: Hasher[_]): Unit = {
      val h = jl.Float.floatToRawIntBits(value) match {
        case scala.Int.MinValue => 0 // map -0.0f to 0.0f
        case int => int
      }
      hasher += h
    }
  }
  implicit object IeeeFloat extends IeeeFloatHash

  trait FloatStrictHash extends Hash[Float] {
    override def hash(value: Float, hasher: Hasher[_]): Unit = hasher += jl.Float.floatToRawIntBits(value)
  }
  implicit object FloatStrict extends FloatStrictHash

  trait DoubleHash extends Hash[Double] {
    override def hash(value: Double, hasher: Hasher[_]): Unit = hasher += jl.Double.hashCode(value)
  }
  implicit object Double extends DoubleHash

  trait IeeeDoubleHash extends Hash[Double] {
    override def hash(value: Double, hasher: Hasher[_]): Unit = {
      val h = jl.Double.doubleToRawLongBits(value) match {
        case scala.Long.MinValue => 0 // map -0.0 to 0.0
        case long => long
      }
      hasher += h
    }
  }
  implicit object IeeeDouble extends IeeeDoubleHash

  trait DoubleStrictHash extends Hash[Double] {
    override def hash(value: Double, hasher: Hasher[_]): Unit = hasher += jl.Double.doubleToRawLongBits(value)
  }
  implicit object DoubleStrict extends DoubleStrictHash

  /* core types */
  trait StringHash extends Hash[String] {
    override def hash(value: String, hasher: Hasher[_]): Unit = hasher += value.getBytes(StandardCharsets.UTF_8)
  }
  implicit object String extends StringHash

  implicit def optionHash[A](implicit hash: Hash[A]): Hash[Option[A]] =
    (option, hasher) =>
      if (option.isEmpty) hasher += noneConstant
      else {
        hasher += someConstant
        hash.hash(option.get, hasher)
      }

  implicit def eitherHash[L, R](implicit hashL: Hash[L], hashR: Hash[R]): Hash[Either[L, R]] =
    (either, hasher) => either match {
      case Left(left) =>
        hasher += leftConstant
        hashL.hash(left, hasher)
      case Right(right) =>
        hasher += rightConstant
        hashR.hash(right, hasher)
    }

  implicit val byteArrayHash: Hash[Array[Byte]] = (value, hasher) => hasher += value

  implicit def arrayHash[A](implicit hash: Hash[A]): Hash[Array[A]] =
    (array, hasher) => for (elem <- array) hash.hash(elem, hasher)

  /* tuples */
  implicit def tuple2[T1, T2](implicit h1: Hash[T1], h2: Hash[T2]): Hash[(T1, T2)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
    }

  implicit def tuple3[T1, T2, T3](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3]): Hash[(T1, T2, T3)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
      h3.hash(value._3, hasher)
    }

  implicit def tuple4[T1, T2, T3, T4](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4]): Hash[(T1, T2, T3, T4)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
      h3.hash(value._3, hasher)
      h4.hash(value._4, hasher)
    }

  implicit def tuple5[T1, T2, T3, T4, T5](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5]): Hash[(T1, T2, T3, T4, T5)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
      h3.hash(value._3, hasher)
      h4.hash(value._4, hasher)
      h5.hash(value._5, hasher)
    }

  implicit def tuple6[T1, T2, T3, T4, T5, T6](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6]): Hash[(T1, T2, T3, T4, T5, T6)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
      h3.hash(value._3, hasher)
      h4.hash(value._4, hasher)
      h5.hash(value._5, hasher)
      h6.hash(value._6, hasher)
    }

  implicit def tuple7[T1, T2, T3, T4, T5, T6, T7](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6], h7: Hash[T7]): Hash[(T1, T2, T3, T4, T5, T6, T7)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
      h3.hash(value._3, hasher)
      h4.hash(value._4, hasher)
      h5.hash(value._5, hasher)
      h6.hash(value._6, hasher)
      h7.hash(value._7, hasher)
    }

  implicit def tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6], h7: Hash[T7], h8: Hash[T8]): Hash[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
      h3.hash(value._3, hasher)
      h4.hash(value._4, hasher)
      h5.hash(value._5, hasher)
      h6.hash(value._6, hasher)
      h7.hash(value._7, hasher)
      h8.hash(value._8, hasher)
    }

  implicit def tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6], h7: Hash[T7], h8: Hash[T8], h9: Hash[T9]): Hash[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    (value, hasher) => {
      hasher += tupleConstant
      h1.hash(value._1, hasher)
      h2.hash(value._2, hasher)
      h3.hash(value._3, hasher)
      h4.hash(value._4, hasher)
      h5.hash(value._5, hasher)
      h6.hash(value._6, hasher)
      h7.hash(value._7, hasher)
      h8.hash(value._8, hasher)
      h9.hash(value._9, hasher)
    }

  object Implicits {
    implicit def hashCodeHash[T <: AnyRef]: Hash[T] = (value, hasher) => hasher += value.hashCode()
  }

  private[this] val noneConstant = -240160639
  private[this] val someConstant = 1009543826
  private[this] val leftConstant = -578715776
  private[this] val rightConstant = 1738541841
  private[this] val tupleConstant = -879856485
}
