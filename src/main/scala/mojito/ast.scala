package mojito

import cats.data.NonEmptyList

object ast {
  sealed trait Definition extends Product with Serializable

  sealed trait ExecutableDefinition extends Definition

  final case class Operation(
    opType: String,
    name: Option[String],
    // variableDefinitions: Option[String],
    directives: Option[NonEmptyList[Directive]],
    selectionSet: NonEmptyList[Field]
  ) extends ExecutableDefinition

  final case class Argument(name: String, value: Value)

  final case class Directive(name: String, arguments: Option[NonEmptyList[Argument]])

  final case class Field(
    alias: Option[String],
    name: String,
    arguments: Option[NonEmptyList[Argument]],
    directives: Option[NonEmptyList[Directive]],
    selectionSet: Option[NonEmptyList[Field]]
  )

  final case class Document(definitions: NonEmptyList[Definition])

  sealed trait Value extends Product with Serializable

  final case class IntValue(value: BigInt) extends Value

  final case class FloatValue(value: BigDecimal) extends Value

  final case class StringValue(name: String) extends Value

  final case class Variable(name: String) extends Value

  final case class BoolValue(bool: Boolean) extends Value

  case object NullValue extends Value

  final case class EnumValue(name: String) extends Value

  final case class ListValue(values: List[Value]) extends Value

  final case class ObjectValue(values: Map[String, Value]) extends Value
}
