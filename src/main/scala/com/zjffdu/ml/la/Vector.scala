package com.zjffdu.ml.la

/**
 * Dense Vector
 */
class Vector(array: Array[Double]) {

  private val innerArray = Array.ofDim[Double](array.length)
  Array.copy(array, 0, innerArray, 0, array.length)

  val length = innerArray.length

  def this(n: Int) = this(Array.ofDim[Double](n))

  def apply(index: Int): Double = {
    if (index < 0 || index > length - 1) {
      throw new Exception("Index outof bound:" + index)
    } else {
      innerArray(index)
    }
  }

  def update(index: Int, value: Double): Unit = {
    innerArray(index) = value
  }

  def *(scalar: Double): Vector = {
    val myarray = innerArray.map(_ * scalar)
    new Vector(myarray)
  }

  def +(other: Vector): Vector = {
    val result = new Vector(length)
    for (i <- (0 to length - 1)) {
      result(i) = apply(i) + other(i)
    }
    result
  }

  def -(other: Vector): Vector = {
    val result = new Vector(length)
    for (i <- (0 to length - 1)) {
      result(i) = apply(i) - other(i)
    }
    result
  }

  def map(f: Double => Double): Vector = {
    val result = new Vector(length)
    for (i <- 0 to (length - 1)) {
      result(i) = f(apply(i))
    }
    result
  }

  def normalizeVector: Vector = {
    val result = new Vector(length)
    for (i <- 0 to length - 1) {
      result(i) = apply(i) / sum
    }
    result
  }

  def min: Double = innerArray.min

  def max: Double = innerArray.max

  def sum: Double = innerArray.sum

  override def toString: String = innerArray.mkString("[", ", ", "]")
}

object Vector {

  def apply(array: Array[Double]) = new Vector(array)

  def main(args: Array[String]) {
    val v1 = new Vector(Array(0.25, 0.25, 0.25, 0.25))
    val v2 = new Vector(Array(0.425, 0.2125, 0.0, 0.425))

//    println(v1 - v2)
//    println((v1 - v2).map(e => Math.abs(e)))
    println(v1+v2)
  }
}