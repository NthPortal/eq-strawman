package com.nthportal.strawman

package object eql {
  implicit final class RichAnyEql[A](val self: A) extends AnyVal {
    def ===(that: A)(implicit eql: Eql[A]): Boolean = eql.equal(self, that)
    def =!=(that: A)(implicit eql: Eql[A]): Boolean = eql.notEqual(self, that)
  }
}
