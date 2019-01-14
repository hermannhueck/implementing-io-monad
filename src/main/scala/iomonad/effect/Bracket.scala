package iomonad.typeclasses

import cats.MonadError

import scala.language.higherKinds

trait Bracket[F[_], E] extends MonadError[F, E] {

  def bracket[A, B](acquire: F[A])(use: A => F[B])(release: A => F[Unit]): F[B]
}
