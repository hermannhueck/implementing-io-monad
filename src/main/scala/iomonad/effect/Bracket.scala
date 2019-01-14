package iomonad.effect

import cats.MonadError

import scala.language.higherKinds

trait Bracket[F[_], E] extends MonadError[F, E] {

  // Operation meant for specifying tasks with safe resource acquisition and release in the face of errors and interruption.
  // This operation provides the equivalent of try/catch/finally statements in mainstream imperative languages for resource acquisition and release.
  def bracket[A, B](acquire: F[A])(use: A => F[B])(release: A => F[Unit]): F[B]

  // Executes the given finalizer when the source is finished, either in success or in error, or if canceled.
  def guarantee[A](fa: F[A])(finalizer: F[Unit]): F[A] = bracket(unit)(_ => fa)(_ => finalizer)
}


/*
sealed abstract class ExitCase[+E] extends Product with Serializable

object ExitCase {

  final case object Completed extends ExitCase[Nothing]
  final case class Error[+E](e: E) extends ExitCase[E]
  final case object Canceled extends ExitCase[Nothing]

  def complete[E]: ExitCase[E] = Completed
  def error[E](e: E): ExitCase[E] = Error[E](e)
  def canceled[E]: ExitCase[E] = Canceled

  def attempt[E, A](value: Either[E, A]): ExitCase[E] =
    value match {
      case Left(e) => ExitCase.error(e)
      case Right(_) => ExitCase.complete
    }
}
*/