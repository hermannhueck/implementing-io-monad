package iomonad.effect

import scala.language.higherKinds
import scala.util.Either

trait Concurrent[F[_]] extends Async[F] {

  def start[A](fa: F[A]): F[Fiber[F, A]]

  def racePair[A,B](fa: F[A], fb: F[B]): F[Either[(A, Fiber[F, B]), (Fiber[F, A], B)]]

  def race[A, B](fa: F[A], fb: F[B]): F[Either[A, B]] =
    flatMap(racePair(fa, fb)) {
      case Left((a, fiberB)) => map(fiberB.cancel)(_ => Left(a))
      case Right((fiberA, b)) => map(fiberA.cancel)(_ => Right(b))
    }

  def cancelable[A](k: (Either[Throwable, A] => Unit) => CancelToken[F]): F[A]
}
