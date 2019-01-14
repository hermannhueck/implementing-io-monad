package iomonad.typeclasses

import scala.language.higherKinds

trait Sync[F[_]] extends Bracket[F, Throwable] {
  /**
    * Suspends the evaluation of an `F` reference.
    *
    * Equivalent to `FlatMap.flatten` for pure expressions,
    * the purpose of this function is to suspend side effects
    * in `F`.
    */
  def suspend[A](thunk: => F[A]): F[A]

  /**
    * Alias for `suspend` that suspends the evaluation of
    * an `F` reference and implements `cats.Defer` typeclass.
    */
  final def defer[A](fa: => F[A]): F[A] = suspend(fa)

  /**
    * Lifts any by-name parameter into the `F` context.
    *
    * Equivalent to `Applicative.pure` for pure expressions,
    * the purpose of this function is to suspend side effects
    * in `F`.
    */
  def delay[A](thunk: => A): F[A] = suspend(pure(thunk))
}
