package mojito

import atto.Atto._
import atto._
import cats.data.NonEmptyList
import cats.implicits._
import mojito.ast._

object parser extends DocumentParsers

trait BasicParsers {
  def braces[A](p: => Parser[A]): Parser[A] = bracket(char('{'), p, char('}')) named "braces"

  def parens[A](p: => Parser[A]): Parser[A] = bracket(char('('), p, char(')')) named "parens"

  def squareBrackets[A](p: => Parser[A]): Parser[A] = bracket(char('['), p, char(']')) named "square brackets"

  // Like atto's built in manyUtil, but doesn't discard until value
  def manyUntil[A](p: Parser[A], q: Parser[A]): Parser[List[A]] = {
    lazy val scan: Parser[List[A]] = q -| (_ :: Nil) | cons(p, scan) -| (_.toList)
    scan named "manyUntil"
  }

  def isWhiteSpace(c: Char): Boolean = c === ' ' || c === '\t'

  lazy val sourceChar: Parser[Char] = tab | newline | carriageReturn | charRange(' ' to '\uFFFF')

  lazy val colon: Parser[Char] = token(char(':'))

  lazy val digit: Parser[Char] = charRange('0' to '9')

  lazy val nonZeroDigit: Parser[Char] = charRange('1' to '9') named "non-zero digit"

  lazy val sign: Parser[Int] = char('-') >| -1 | ok(1) named "negative sign"

  lazy val name: Parser[String] = token(nameFirst ~ stringOf(nameRest) -| { case (a, b) => a.toString + b } named "name")

  lazy val keywords: NonEmptyList[String] = NonEmptyList.of("true", "false", "null")

  private def token[A](p: Parser[A]) = p <~ ignoredSpace named "token"

  private def bracket[A, B, C](left: Parser[B], p: => Parser[A], right: => Parser[C]) = token(left) ~> p <~ token(right)

  private val tab = char('\t')

  private val newline = char('\n')

  private val carriageReturn = char('\r')

  private val unicodeBom = char('\uFEFF')

  private val whitespace = satisfy(isWhiteSpace)

  private val lineTerminator = string("\r\n") | stringOf1(newline)

  private val commentChar = tab | charRange(' ' to '\uFFFF')

  private val comment = char('#') ~ stringOf(commentChar) -| { case (a, b) => a.toString + b } named "comment"

  private val comma = char(',')

  private val ignored = stringOf1(unicodeBom | whitespace | comma) | lineTerminator | comment named "ignored"

  private val ignoredSpace = many(ignored).void named "ignored space"

  private val underscore = char('_')

  private val letter = charRange('A' to 'Z') | charRange('a' to 'z')

  private val nameFirst = underscore | letter

  private val nameRest = nameFirst | digit
}

trait ValueParsers extends BasicParsers {
  lazy val value: Parser[Value] = {
    floatValue.widen[Value] |
    intValue.widen[Value] |
    stringValue.widen[Value] |
    variable.widen[Value] |
    enumValue.widen[Value] |
    booleanValue.widen[Value] |
    nullValue.widen[Value] |
    listValue.widen[Value] |
    objectValue.widen[Value]
  } named "value"

  private val signNum = (sign, nonZeroDigit, stringOf(digit)).mapN((s, nz, d) => s * BigInt(nz.toString + d)) named "signed number"

  private val intPart = sign ~> char('0') >| BigInt(0) | signNum named "int part"

  private val fracPart = char('.') ~> stringOf1(digit) -| BigInt.apply

  private val expIndicator = char('e') | char('E')

  private val expPart = (expIndicator ~> sign, stringOf1(digit)).mapN(_ * BigInt(_))

  private val variable = token(char('$')) ~> name -| Variable.apply named "variable"

  private val intValue = token(intPart -| IntValue.apply) named "integer value"

  private val floatPart = (intPart, fracPart).mapN((i, f) => BigDecimal(s"$i.$f"))

  private val number = (floatPart | (intPart -| BigDecimal.apply), expPart).mapN((i, b) => BigDecimal(s"${i}e$b"))

  private val floatValue = token((number | floatPart) -| FloatValue.apply) named "float value"

  private val unicode = string("\\u") ~> count(4, hexDigit) -| (c => Integer.parseInt(new String(c.toArray), 16).toChar)

  private val escapeChars = NonEmptyList.of('"', '\\', '/', '\b', '\f', '\n', '\r', '\t')

  private val escapedChar = char('\\') ~> escapeChars.map(char).reduceLeft(_ | _)

  private val nonStringChars = NonEmptyList.of('\\', '"', '\r', '\n')

  private val quotation = char('"')

  private val stringChar = sourceChar.filter(c => !nonStringChars.contains_(c)) | unicode | escapedChar

  private val quotedString = quotation ~> stringOf(stringChar) <~ quotation

  private val blockQuote = "\"\"\""

  private val escapedBlockQuote = "\\" + blockQuote

  private val blockString = manyUntil(sourceChar -| (_.toString), escapedBlock | string(blockQuote) >| "") -| (_.mkString) named "block string"

  private lazy val escapedBlock: Parser[String] = string(escapedBlockQuote) *> (ok(blockQuote), blockString).mapN(_ + _)

  private def blockStringValue(raw: String) = {
    def isBlank(str: String) = str.forall(isWhiteSpace)

    val lines = NonEmptyList.fromListUnsafe(raw.split("""\r\n|[\n\r]""").toList)
    val commonIndent = lines.tail.filterNot(isBlank).map(_.prefixLength(isWhiteSpace)).minimumOption.getOrElse(0)
    val adjustedLines = lines.head :: lines.tail.map(_.drop(commonIndent))
    adjustedLines.dropWhile_(isBlank).takeWhile_(!isBlank(_)).mkString("\n")
  }

  private val blockQuotedString = string(blockQuote) ~> blockString -| blockStringValue named "block quote"

  private val stringValue = token(blockQuotedString | quotedString) -| StringValue.apply named "string value"

  private val enumValue = name.filter(a => !keywords.contains_(a)) -| EnumValue.apply named "enum value"

  private val booleanValue = token((string("true") >| true | string("false") >| false) -| BoolValue.apply) named "boolean value"

  private val nullValue = token(string("null") >| NullValue) named "null value"

  private lazy val listValue = squareBrackets(many(value)) -| ListValue.apply named "list value"

  private val objectField = (name <~ colon, value).mapN(_ -> _) named "object field"

  private val objectValue = braces(many(objectField)).map(_.toMap) -| ObjectValue.apply named "object value"
}

trait ArgumentParsers extends ValueParsers with BasicParsers {
  lazy val arguments: Parser[NonEmptyList[Argument]] = parens(many1(argument)) named "arguments"

  private val argument = (name <~ colon, value).mapN(Argument.apply) named "argument"
}

trait DirectiveParsers extends ArgumentParsers with BasicParsers {
  lazy val directives: Parser[NonEmptyList[Directive]] = many1(directive) named "directives"

  private val directive = (token(char('@')) ~> name, opt(arguments)).mapN(Directive.apply) named "directive"
}

trait SelectionSetParsers extends DirectiveParsers with BasicParsers {
  lazy val selectionSet: Parser[NonEmptyList[Field]] = braces(many1(selection)) named "selection set"

  private val alias = name <~ colon named "alias"

  private val field: Parser[Field] = (
    opt(alias),
    name,
    opt(arguments),
    opt(directives),
    opt(selectionSet)
  ).mapN(Field.apply) named "field"

  private val selection = field named "selection" //| fragmentSpread | inlineFragment
}

trait DocumentParsers extends DirectiveParsers with SelectionSetParsers with BasicParsers {
  lazy val document: Parser[Document] = many1(definition) -| Document.apply named "document"

  private val operationType = token(string("query") | string("mutation") | string("subscription")) named "operation type"

  private val operationDefinition = (
    operationType,
    opt(name),
    //    opt(variableDefinitions),
    opt(directives),
    selectionSet
  ).mapN(Operation.apply) | selectionSet -| (ss => Operation("query", None, None, ss)) named "operation"

  private val executableDefinition: Parser[ExecutableDefinition] = operationDefinition -| (e => e: ExecutableDefinition) named "executable definition" //| fragmentDefinition

  private val definition: Parser[Definition] = executableDefinition -| (d => d: Definition) named "definition" // | typeSystemDefinition | typeSystemExtension
}
