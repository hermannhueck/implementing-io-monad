package littleMonadDemo

import littleMonadDemo.monad._
import littleMonadDemo.id._

package object computeInts {

  def compute(i1: Int, i2: Int): (Int, Int) =
    compute(i1: Id[Int], i2: Id[Int])

  def compute[F[_]: Monad](fInt1: F[Int], fInt2: F[Int]): F[(Int, Int)] =
    for {
      i1 <- fInt1
      i2 <- fInt2
    } yield (i1, i2)

  def compute2[F[_]: Monad](fInt1: F[Int], fInt2: F[Int]): F[(Int, Int)] =
    fInt1.flatMap { i1 =>
      fInt2.map { i2 =>
        (i1, i2)
      }
    }
}
