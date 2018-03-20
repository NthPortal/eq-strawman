package com.nthportal.strawman.eql

trait Hasher[R] {
  def state: HashState
  def reset(): Unit
  def result(): R
}

object Hasher {
  class IntHasher extends Hasher[Int] {
    import IntHasher._

    private object State extends HashState {
      private[IntHasher] var value: Int = 0

      override def +=(byte: Byte): this.type = {
        value = value * p1 + byte * p2
        this
      }

      override def +=(int: Int): this.type = {
        value = value * p1 + int * p2
        this
      }

      override def +=(long: Long): this.type = {
        val tmp = long * p2
        value = value * p1 + ((tmp >>> Integer.SIZE) ^ tmp).toInt
        this
      }

      override def +=(bytes: Array[Byte]): this.type = {
        for (b <- bytes) {
          value = value * p1 + b * p2
        }
        this
      }
    }

    override def state: HashState = State
    override def reset(): Unit = State.value = 0
    override def result(): Int = State.value
  }

  object IntHasher {
    def apply(): IntHasher = new IntHasher

    private final val p1 = 37
    private final val p2 = 19
  }
}
