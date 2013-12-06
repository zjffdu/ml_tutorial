package com.zjffdu.ml.la

class Matrix(nRow: Int, nCol: Int) {

  val innerArray = Array.ofDim[Double](nRow * nCol)

  def apply(i: Int, j: Int): Double = {
    if (i < 0 || i >= nRow) {
      throw new Exception("row index is out of boundary:" + i)
    }
    if (j < 0 || j >= nCol) {
      throw new Exception("col index is out of boundary:" + j)
    }
    innerArray(i * nRow + j)
  }

  def update(i: Int, j: Int, value: Double) {
    if (i < 0 || i >= nRow) {
      throw new Exception("row index is out of boundary:" + i)
    }
    if (j < 0 || j >= nCol) {
      throw new Exception("col index is out of boundary:" + j)
    }
    innerArray(i * nRow + j) = value
  }

  def sumOfColumn(j: Int): Double = (0 to nRow - 1).map(i => apply(i, j)).sum

  def sumOfRow(i: Int): Double = (0 to nCol - 1).map(j => apply(i, j)).sum

  def *(v: Vector): Vector = {
    if (nCol != v.length) {
      throw new Exception("You can not multiply Matrix[$nRow,$nCol] with Vector[" + v.length + "]")
    }
    val resultInnerArray = (0 to nRow - 1).map(i => {
      (0 to nCol - 1).map(j => apply(i, j) * v(j)).sum
    }).toArray
    Vector(resultInnerArray)
  }

  def *(value: Double): Matrix = {
    val matrix = new Matrix(nRow, nCol)
    for (
      i <- (0 to nRow - 1);
      j <- (0 to nCol - 1)
    ) {
      matrix(i, j) = apply(i, j) * value
    }
    matrix
  }

  override def toString = {
    (0 to nRow - 1).map(i => {
      (0 to nCol - 1).map(j => apply(i, j)).mkString(", ")
    }).mkString("\n")
  }
}

object Matrix {

  def main(args: Array[String]) {
    val m = new Matrix(3, 3)
    m(0, 1) = 1
    m(0, 2) = 1
    m(1, 0) = 0.5
    m(2, 0) = 0.5

    println(m)

    val v = new Vector(Array(1.0 / 3, 1.0 / 3, 1.0 / 3))
    //    println(v)
    //    println(v * 0.85)

    val result = m * v * 0.85 + v * (1 - 0.85)
    println(result)
  }
}