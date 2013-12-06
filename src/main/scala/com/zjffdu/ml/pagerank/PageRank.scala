package com.zjffdu.ml.pagerank

import com.zjffdu.ml.la._
import scala.collection.mutable.ListBuffer
import java.io.File
import scala.util.control.Breaks._

class Graph {
  val nodes = scala.collection.mutable.Set[Int]()
  val edges = scala.collection.mutable.Map[Int, ListBuffer[Int]]()

  def addEdge(source: Int, dest: Int): Unit = {
    nodes.add(source)
    nodes.add(dest)
    edges.get(source) match {
      case Some(e) => e.append(dest)
      case None => edges.put(source, ListBuffer(dest))
    }
  }

  def f = None
}

object Graph {
  def apply(file: File): Graph = {
    val g = new Graph
    scala.io.Source.fromFile(file).getLines.foreach(line => {
      val tokens = line.split("\\s")
      val source = tokens(0).toInt
      val dest = tokens(1).toInt
      g.addEdge(source, dest)
    })
    g
  }
}

object PageRank {


  def iterative(g: Graph, maxIteration:Int=10, damp:Double=0.85,theta:Double=0.001) {
    val n = g.nodes.size
    val v = Array.ofDim[Vector](maxIteration)
    v(0) = initV(n)
    val M = buildMatrix(g)
    val II = II_Vector(n)

    println("Transition Matrix:")
    println(M)
    
    println("PageRank:" + v(0))
    var finalIndex = 0
    breakable {
      (0 to maxIteration - 2).foreach(i => {
        v(i + 1) = M * v(i) * damp + II * (1 - damp)
        v(i + 1) = v(i + 1).normalizeVector
        val theta = (v(i + 1) - v(i)).map(e => Math.abs(e)).max
        finalIndex = i + 1
        println("Iteration:"+(i+1))
        println("PageRank:"+v(i+1))
        println("Theta:" + theta)
        if (theta < 0.01)
          break
      })
    }
  }

  def initV(n: Int): Vector = {
    val array = Array.fill(n)(1.0 / n)
    Vector(array)
  }

  def buildMatrix(g: Graph): Matrix = {
    val nodes = g.nodes.toList
    val n = nodes.size
    val matrix = new Matrix(n, n)
    // init matrix
    g.edges.foreach(e => {
      val source = e._1
      e._2.foreach(dest => {
        val i = nodes.indexOf(source)
        val j = nodes.indexOf(dest)
        matrix(i, j) = 1
      })
    })
    // normalize matrix
    for (col <- 0 to n - 1) {
      val sumOfColumn = matrix.sumOfColumn(col)
      for (row <- 0 to n - 1) {
        matrix(row, col) = matrix(row, col) / sumOfColumn
      }
    }

    matrix
  }

  def II_Vector(n: Int): Vector = {
    val vector = new Vector(n)
    for (i <- (0 to n - 1)) {
      vector(i) = 1.0 / n
    }
    vector
  }

  def main(args: Array[String]) {
    val g = Graph(new File("data/pagerank/sample3.txt"))
    iterative(g)
  }
}