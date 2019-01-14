package iomonad.effect

import scala.language.higherKinds
import scala.util.Either

trait Async[F[_]] extends Sync[F] {

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]

  def never[A]: F[A] = async(_ => ())
}
