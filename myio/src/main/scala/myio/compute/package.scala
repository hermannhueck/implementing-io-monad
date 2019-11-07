package myio

import scala.annotation.tailrec

package object compute {

  final def sumOfRange(from: Int, to: Int): Int =
    (from until to).toList.sum

  @tailrec
  final def fibonacci(cycles: Int, a: BigInt = 0, b: BigInt = 1): BigInt = {
    if (cycles > 0)
      fibonacci(cycles - 1, b, a + b)
    else
      b
  }

  def factorial(n: Int): BigInt = {

    @tailrec
    def fac(n2: Int, acc: BigInt): BigInt =
      if (n2 == 0)
        acc
      else
        fac(n2 - 1, n2 * acc)

    if (n < 0)
      throw new IllegalArgumentException(s"factorial of $n not defined")
    else
      fac(n, BigInt(1))
  }
}
