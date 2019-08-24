package mojito.free

import cats.data.Kleisli
import mojito.json.Json
import cats.implicits._
import cats.Applicative
import cats.Functor

sealed trait GqlField[F[_], A] {

  def name: String

  def resolve: Kleisli[F, A, Json]

  final case class Entry[A](name: String, resolver: Kleisli[F, A, Json])

  object Entry {
    implicit val functor: Functor[Entry] = ???
  }
}

object GqlField {

}

final case class GqlObj[F[_] : Applicative, A](name: String, fields: List[(String, Kleisli[F, A, Json])]) extends GqlField[F, A] {

  def resolve: Kleisli[F, A, Json] = {
    val foo = fields.traverse(_.sequence) // TODO: fold to json object
    ???
  }
}

object GqlObj {
   type RootObj[F[_]] = GqlObj[F, Unit]
 }

final case class GqlPrimitive[F[_], A](name: String, resolve: Kleisli[F, A, Json]) extends GqlField[F, A]