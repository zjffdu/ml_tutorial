package com.zjffdu.ml.la

class Scalar(value: Double) {

  def *(vector: Vector): Vector = vector * value

  def *(matrix: Matrix): Matrix = matrix * value
  
}