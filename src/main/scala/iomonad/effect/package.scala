package iomonad


package object effect {

  type CancelToken[F[_]] = F[Unit]
}
