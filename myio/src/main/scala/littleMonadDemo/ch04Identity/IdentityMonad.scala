package littleMonadDemo.ch04Identity

import littleMonadDemo.computeInts._

object IdentityMonad extends util.App {

  println("----- Identity:")

  val idResult = compute(42, 42)
  println(idResult)
}
