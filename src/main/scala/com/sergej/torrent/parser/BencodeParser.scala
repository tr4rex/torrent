package com.sergej.torrent.parser

import scala.util.parsing.combinator.RegexParsers

object BencodeParser extends RegexParsers {

  override def skipWhitespace: Boolean = false


  private def anytype: Parser[Any] = int | string | list | dict

  private def int: Parser[Int] = "i" ~> ("""-?\d+""".r ^^ (_.toInt)) <~ "e"

  private def string: Parser[String] = ("""\d+""".r <~ ":") >> { len => s".{$len}".r }

  private def list: Parser[List[Any]] = "l" ~> anytype.* <~ "e"

  private def dict: Parser[Map[String, Any]] = "d" ~> (string ~ anytype).* <~ "e" ^^ (_.map { case x ~ y => (x, y) }.toMap)


  def parser: Parser[Any] = anytype

  def apply(input: String): BencodeParser.ParseResult[Any] = BencodeParser.parseAll(BencodeParser.parser, input)
}
