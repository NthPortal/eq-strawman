package com.nthportal.strawman.eql

trait HashFactory[R, H <: Hasher[R]] {
  def newHasher: H

  def hash[T: Hash](value: T): R = (newHasher += value).result()
}

object HashFactory {
  abstract class Caching[R, H <: Hasher[R]] extends HashFactory[R, H] {
    private[this] val cache: ThreadLocal[H] = new ThreadLocal[H] {
      override def initialValue() = newHasher
    }

    override def hash[T: Hash](value: T): R = {
      val hasher = cache.get()
      hasher.reset()
      (hasher += value).result()
    }
  }
}
