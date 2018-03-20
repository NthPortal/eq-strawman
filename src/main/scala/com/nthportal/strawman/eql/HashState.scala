package com.nthportal.strawman.eql

trait HashState {
  def +=(byte: Byte): this.type
  def +=(int: Int): this.type
  def +=(long: Long): this.type
  def +=(bytes: Array[Byte]): this.type
}
