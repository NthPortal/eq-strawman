package com.nthportal.strawman.eql

import java.nio.charset.StandardCharsets
import java.{lang => jl}

trait Hash[T] {
  def hash(value: T, state: HashState): Unit
}

object Hash {
  @inline def apply[T: Hash]: Hash[T] = implicitly[Hash[T]]

  /* primitives */
  trait UnitHash extends Hash[Unit] {
    override def hash(value: Unit, state: HashState): Unit = state += 0
  }
  implicit object Unit extends UnitHash

  implicit object Nothing extends Hash[Nothing] {
    override def hash(value: Nothing, state: HashState): Unit = throw new AssertionError("can't hash nothing")
  }

  trait ByteHash extends Hash[Byte] {
    override def hash(value: Byte, state: HashState): Unit = state += value
  }
  implicit object Byte extends ByteHash

  trait ShortHash extends Hash[Short] {
    override def hash(value: Short, state: HashState): Unit = state += value.toInt
  }
  implicit object Short extends ShortHash

  trait IntHash extends Hash[Int] {
    override def hash(value: Int, state: HashState): Unit = state += value
  }
  implicit object Int extends IntHash

  trait LongHash extends Hash[Long] {
    override def hash(value: Long, state: HashState): Unit = state += value
  }
  implicit object Long extends LongHash

  trait CharHash extends Hash[Char] {
    override def hash(value: Char, state: HashState): Unit = state += value.toInt
  }
  implicit object Char extends CharHash

  /* floating point, ugh */
  trait FloatHash extends Hash[Float] {
    override def hash(value: Float, state: HashState): Unit = state += jl.Float.hashCode(value)
  }
  implicit object Float extends FloatHash

  trait IeeeFloatHash extends Hash[Float] {
    override def hash(value: Float, state: HashState): Unit = {
      val h = jl.Float.floatToRawIntBits(value) match {
        case scala.Int.MinValue => 0 // map -0.0f to 0.0f
        case int => int
      }
      state += h
    }
  }
  implicit object IeeeFloat extends IeeeFloatHash

  trait FloatStrictHash extends Hash[Float] {
    override def hash(value: Float, state: HashState): Unit = state += jl.Float.floatToRawIntBits(value)
  }
  implicit object FloatStrict extends FloatStrictHash

  trait DoubleHash extends Hash[Double] {
    override def hash(value: Double, state: HashState): Unit = state += jl.Double.hashCode(value)
  }
  implicit object Double extends DoubleHash

  trait IeeeDoubleHash extends Hash[Double] {
    override def hash(value: Double, state: HashState): Unit = {
      val h = jl.Double.doubleToRawLongBits(value) match {
        case scala.Long.MinValue => 0 // map -0.0 to 0.0
        case long => long
      }
      state += h
    }
  }
  implicit object IeeeDouble extends IeeeDoubleHash

  trait DoubleStrictHash extends Hash[Double] {
    override def hash(value: Double, state: HashState): Unit = state += jl.Double.doubleToRawLongBits(value)
  }
  implicit object DoubleStrict extends DoubleStrictHash

  /* core types */
  trait StringHash extends Hash[String] {
    override def hash(value: String, state: HashState): Unit = state += value.getBytes(StandardCharsets.UTF_8)
  }
  implicit object String extends StringHash

  implicit def optionHash[A](implicit hash: Hash[A]): Hash[Option[A]] =
    (option, state) => option match {
      case None => state += noneConstant
      case Some(value) =>
        state += someConstant
        hash.hash(value, state)
    }

  implicit def eitherHash[L, R](implicit hashL: Hash[L], hashR: Hash[R]): Hash[Either[L, R]] =
    (either, state) => either match {
      case Left(left) =>
        state += leftConstant
        hashL.hash(left, state)
      case Right(right) =>
        state += rightConstant
        hashR.hash(right, state)
    }

  implicit val byteArrayHash: Hash[Array[Byte]] = (value, state) => state += value

  implicit def arrayHash[A](implicit hash: Hash[A]): Hash[Array[A]] =
    (array, state) => for (elem <- array) hash.hash(elem, state)

  implicit def iterableHash[A](implicit hash: Hash[A]): Hash[Iterable[A]] =
    (iterable, state) => for (elem <- iterable) hash.hash(elem, state)

  /* tuples */
  implicit def tuple2[T1, T2](implicit h1: Hash[T1], h2: Hash[T2]): Hash[(T1, T2)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
    }

  implicit def tuple3[T1, T2, T3](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3]): Hash[(T1, T2, T3)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
      h3.hash(value._3, state)
    }

  implicit def tuple4[T1, T2, T3, T4](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4]): Hash[(T1, T2, T3, T4)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
      h3.hash(value._3, state)
      h4.hash(value._4, state)
    }

  implicit def tuple5[T1, T2, T3, T4, T5](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5]): Hash[(T1, T2, T3, T4, T5)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
      h3.hash(value._3, state)
      h4.hash(value._4, state)
      h5.hash(value._5, state)
    }

  implicit def tuple6[T1, T2, T3, T4, T5, T6](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6]): Hash[(T1, T2, T3, T4, T5, T6)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
      h3.hash(value._3, state)
      h4.hash(value._4, state)
      h5.hash(value._5, state)
      h6.hash(value._6, state)
    }

  implicit def tuple7[T1, T2, T3, T4, T5, T6, T7](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6], h7: Hash[T7]): Hash[(T1, T2, T3, T4, T5, T6, T7)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
      h3.hash(value._3, state)
      h4.hash(value._4, state)
      h5.hash(value._5, state)
      h6.hash(value._6, state)
      h7.hash(value._7, state)
    }

  implicit def tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6], h7: Hash[T7], h8: Hash[T8]): Hash[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
      h3.hash(value._3, state)
      h4.hash(value._4, state)
      h5.hash(value._5, state)
      h6.hash(value._6, state)
      h7.hash(value._7, state)
      h8.hash(value._8, state)
    }

  implicit def tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit h1: Hash[T1], h2: Hash[T2], h3: Hash[T3], h4: Hash[T4], h5: Hash[T5], h6: Hash[T6], h7: Hash[T7], h8: Hash[T8], h9: Hash[T9]): Hash[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    (value, state) => {
      state += tupleConstant
      h1.hash(value._1, state)
      h2.hash(value._2, state)
      h3.hash(value._3, state)
      h4.hash(value._4, state)
      h5.hash(value._5, state)
      h6.hash(value._6, state)
      h7.hash(value._7, state)
      h8.hash(value._8, state)
      h9.hash(value._9, state)
    }

  object Implicits {
    implicit def cooperative[T]: Hash[T] = (value, state) => state += value.##
    implicit def hashCode[T <: AnyRef]: Hash[T] = (value, state) =>
      state += (if (value eq null) 0 else value.hashCode())
    implicit def refIdentity[T <: AnyRef]: Hash[T] = (value, state) => state += System.identityHashCode(value)
  }

  private[this] val noneConstant = -240160639
  private[this] val someConstant = 1009543826
  private[this] val leftConstant = -578715776
  private[this] val rightConstant = 1738541841
  private[this] val tupleConstant = -879856485
}
