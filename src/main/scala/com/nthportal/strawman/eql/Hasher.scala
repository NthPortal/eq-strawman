package com.nthportal.strawman.eql

trait Hasher[R] {
  def state: HashState
  def reset(): Unit
  def result(): R
  final def +=[T](t: T)(implicit hash: Hash[T]): this.type = {
    hash.hash(t, state)
    this
  }
}

trait HashState {
  def +=(byte: Byte): this.type
  def +=(int: Int): this.type
  def +=(long: Long): this.type
  def +=(bytes: Array[Byte]): this.type
}

object Hasher {
  class IntHasher extends Hasher[Int] {
    import IntHasher._

    private object State extends HashState {
      private[IntHasher] var value: Int = initial

      override def +=(byte: Byte): this.type = {
        value = value * multiplier + byte
        this
      }

      override def +=(int: Int): this.type = {
        value = value * multiplier + int
        this
      }

      override def +=(long: Long): this.type = {
        value = value * multiplier + java.lang.Long.hashCode(long)
        this
      }

      override def +=(bytes: Array[Byte]): this.type = {
        for (b <- bytes) {
          value = value * multiplier + b
        }
        this
      }
    }

    override def state: HashState = State
    override def reset(): Unit = State.value = initial
    override def result(): Int = State.value
  }

  object IntHasher extends HashFactory.Caching[Int, IntHasher] {
    private final val initial = 17
    private final val multiplier = 31

    def apply(): IntHasher = new IntHasher

    override def newHasher: IntHasher = new IntHasher
  }
}
