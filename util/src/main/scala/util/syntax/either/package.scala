package util.syntax

package object either {

  implicit class EitherOps[+L, +R](private val self: Either[L, R]) extends AnyVal {

    @inline def mapLeft[L2](f: L => L2): Either[L2, R] =
      self
        .swap
        .map(f)
        .swap
  }
}
