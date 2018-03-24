package com.nthportal.strawman.eql

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}

object Values {
  val dNaN1: Double = Double.NaN
  val dNaN2: Double = longBitsToDouble(doubleToRawLongBits(Double.NaN) ^ Long.MinValue)
  val fNaN1: Float = Float.NaN
  val fNaN2: Float = intBitsToFloat(floatToRawIntBits(Float.NaN) ^ Int.MinValue)

  val ints = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)
  val strings = Seq("", "foo", "bar", "baz")
  val floats = Seq(
    Float.NegativeInfinity,
    Float.MinValue,
    -1.0f,
    0.0f,
    Float.MinPositiveValue,
    1.0f,
    Float.MaxValue,
    Float.PositiveInfinity,
  )
  val doubles = Seq(
    Double.NegativeInfinity,
    Double.MinValue,
    -1.0,
    0.0,
    Double.MinPositiveValue,
    1.0,
    Double.MaxValue,
    Double.PositiveInfinity,
  )
}
