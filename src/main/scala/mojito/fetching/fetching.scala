package mojito.fetching

import cats.effect.ConcurrentEffect
import cats.data.NonEmptyList
import fetch.Data

object fetching extends App {

  trait DataSource[F[_], Identity, Result]{
    def data: Data[Identity, Result]

    def CF: ConcurrentEffect[F]

    def fetch(id: Identity): F[Option[Result]]

    /* `batch` is implemented in terms of `fetch` by default */
    def batch(ids: NonEmptyList[Identity]): F[Map[Identity, Result]]
  }

}