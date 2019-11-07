package myio

package object effect {

  type CancelToken[F[_]] = F[Unit]
}
