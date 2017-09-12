package com.sergej.torrent.parser

import org.scalatest._

class BencodeParser$Test extends FlatSpec with Matchers {

  val parser = BencodeParser

  "BencodeParser" should "properly parse positive integer" in {
    val i = 11
    val result = parser(int(i))
    result.successful should be(true)
    result.get should be(i)
  }

  // todo: group simple types testing

  it should "properly parse negative integer" in {
    val i = -11
    val result = parser(int(i))
    result.successful should be(true)
    result.get should be(i)
  }

  it should "properly parser string" in {
    val s = "south park"
    val result = parser(str(s))
    result.successful should be(true)
    result.get should be(s)
  }

  it should "not parse two integers in a row" in {
    val i = 3
    val result = parser(int(i) + int(i))
    result.successful should be(false)
  }

  it should "fail on parsing too short string" in {
    val s = "south park"
    val result = parser(s"${s.length + 10}:$s")
    result.successful should be(false)
  }

  it should "fail on parsing too long string" in {
    val s = "south park"
    val result = parser(s"1:$s")
    result.successful should be(false)
  }

  it should "parse proper list" in {
    val i = 3
    val s = "south park"
    val l = List(i, s)
    val result = parser(list(l))
    result.successful should be(true)
    result.get should be(l)
  }

  it should "parse proper dict" in {
    val ki = "key integer"
    val ks = "key string"
    val kl = "key list"
    val kd = "key dict"
    val vi = 3
    val vs = "south park"
    val vl = List(vi, vs)
    val vd = Map(vs -> vi)
    val d = Map(ki -> vi, ks -> vs, kl -> vl, kd -> vd)
    val result = parser(dict(d))
    result.successful should be(true)
    result.get should be(d)
    println(result.get)
  }

  // helper functions. they also can contain errors :(

  def int(i: Int) = s"i${i}e"

  def str(s: String) = s"${s.length}:$s"

  def list(xs: List[Any]): String = {
    val listStr = xs.map {
      case i: Int => int(i)
      case s: String => str(s)
      case List(xs: List[Any]) => list(xs)
    }.mkString("")
    s"l${listStr}e"
  }

  def dict(d: Map[String, Any]): String = {
    val dictStr = d.map {
      case (k, v) =>
        val vs = v match {
          case i: Int => int(i)
          case s: String => str(s)
          case l: List[Any] => list(l)
          case id: Map[String, Any] => dict(id)
        }
        s"${str(k)}$vs"
    }.mkString("")
    s"d${dictStr}e"
  }
}
