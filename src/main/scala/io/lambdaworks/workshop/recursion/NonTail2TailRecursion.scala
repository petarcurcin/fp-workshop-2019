package io.lambdaworks.workshop.recursion

import scala.annotation.tailrec

/**
  * Rewrite below non tail-recursive functions to tail-recursive one.
  * Add @tailrec annotation to prove it.
  */
object NonTail2TailRecursion {

  def factorial(n: Int): Int = {
    @tailrec
    def factorialRec(n: Int, acc: Int = 1): Int =
      if (n <= 0) acc else factorialRec(n - 1, n * acc)

    factorialRec(n)
  }

  def cubesOfEvens(numbers: List[Double]): List[Double] = {
    @tailrec
    def cubesOfEvensRec(numbers: List[Double], acc: List[Double] = List()): List[Double] = {
      numbers match {
        case x :: xs if x % 2 == 0 => cubesOfEvensRec(xs, Math.pow(x, 3) +: acc)
        case _ :: xs => cubesOfEvensRec(xs, acc)
        case Nil     => acc
      }
    }
    cubesOfEvensRec(numbers)
  }

}
