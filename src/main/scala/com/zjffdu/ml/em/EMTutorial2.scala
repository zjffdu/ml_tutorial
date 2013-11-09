package com.zjffdu.ml.em

/**
 *
 * Implementation of EM Example
 * http://www.nature.com/nbt/journal/v26/n8/full/nbt1406.html
 *
 */
object EMTutorial2 {

  def factorial(n: Int): Int = {
    if (n == 0) {
      1
    } else {
      n * factorial(n - 1)
    }
  }

  def bino_prob(n: Int, k: Int, theta: Double): Double = {
    val coef = factorial(n).toDouble / (factorial(k) * factorial(n - k))
    coef * Math.log(Math.pow(theta, k)) * Math.log(Math.pow(theta, n - k))
  }

  def main(args: Array[String]) {

    var theta_A = 0.7
    var theta_B = 0.5
    var delta = 0.001
    var new_theta_A: Double = theta_A
    var new_theta_B: Double = theta_B

    val inputs = Array(5, 9, 8, 4, 7)

    do {
      theta_A = new_theta_A
      theta_B = new_theta_B

      val newStatus = inputs.map(head => {
        val tail = 10 - head
        val headProb = head / 10.toDouble
        if (Math.abs(headProb - theta_A) < Math.abs(headProb - theta_B)) {
          (head, tail, 0, 0)
        } else {
          (0, 0, head, tail)
        }
      })
        .reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))

      new_theta_A = if ((newStatus._1 + newStatus._2) != 0)
    	  				newStatus._1.toDouble / (newStatus._1 + newStatus._2)
				      else
				        0
      new_theta_B = if ((newStatus._3 + newStatus._4) != 0)
    	  				newStatus._3.toDouble / (newStatus._3 + newStatus._4)
				      else
				        0

      println("new_theta_A:" + new_theta_A + "\tnew_theta_B:" + new_theta_B)

    } while (Math.abs(new_theta_A - theta_A) > delta && Math.abs(new_theta_B - theta_B) > delta)
  }
}