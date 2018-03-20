package com.nthportal.strawman.eql

trait Hasher[R] {
  self =>
  def clear(): Unit
  def +=(byte: Byte): self.type
  def +=(int: Int): self.type
  def +=(long: Long): self.type
  def +=(bytes: Array[Byte]): self.type
  def result(): R
}

object Hasher {
  class IntHasher extends Hasher[Int] {
    import IntHasher._

    private[this] var state: Int = 0

    override def clear(): Unit = state = 0

    override def +=(byte: Byte): this.type = {
      state = state * p1 + byte * p2
      this
    }

    override def +=(int: Int): this.type = {
      state = state * p1 + int * p2
      this
    }

    override def +=(long: Long): this.type = {
      val tmp = long * p2
      state = state * p1 + ((tmp >>> Integer.SIZE) ^ tmp).toInt
      this
    }

    override def +=(bytes: Array[Byte]): this.type = {
      for (b <- bytes) {
        state = state * p1 + b * p2
      }
      this
    }

    override def result(): Int = state
  }

  object IntHasher {
    def apply(): IntHasher = new IntHasher

    private final val p1 = 37
    private final val p2 = 19
  }
}
