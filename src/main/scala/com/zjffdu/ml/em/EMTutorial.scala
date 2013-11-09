package com.zjffdu.ml.em

/**
 *
 * Implementation of EM Example
 * http://www.nature.com/nbt/journal/v26/n8/full/nbt1406.html
 *
 */
object EMTutorial {

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
    var theta_B = 0.1
    var delta = 0.001
    var new_theta_A: Double = theta_A
    var new_theta_B: Double = theta_B

    val inputs = Array(5, 9, 8, 4, 7)

    do {
      theta_A = new_theta_A
      theta_B = new_theta_B

      val newStatus = inputs.map(head => {
        val tail = 10 - head

        val prob_A = bino_prob(10, head, theta_A)
        val prob_B = bino_prob(10, head, theta_B)
        val prob_ANorm = prob_A / (prob_A + prob_B)
        val prob_BNorm = prob_B / (prob_A + prob_B)

        val headA = prob_ANorm * head
        val tailA = prob_ANorm * tail
        val headB = prob_BNorm * head
        val tailB = prob_BNorm * tail
        (headA, tailA, headB, tailB)
      })
      .reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))

      new_theta_A = newStatus._1 / (newStatus._1 + newStatus._2)
      new_theta_B = newStatus._3 / (newStatus._3 + newStatus._4)
      
      println("new_theta_A:" + new_theta_A + "\tnew_theta_B:" + new_theta_B)

    } while (Math.abs(new_theta_A - theta_A) > delta && Math.abs(new_theta_B - theta_B) > delta)
  }
}