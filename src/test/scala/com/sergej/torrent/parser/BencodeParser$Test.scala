package com.sergej.torrent.parser

import org.scalatest._

class BencodeParser$Test extends FlatSpec with Matchers {

  val parser = BencodeParser

  "BencodeParser" should "properly parse positive integer" in {
    val t = parser("i11e")
    t.successful should be (true)
    t.get should be (11)
  }

  it should "properly parse negative integer" in {
    val t = parser("i-11e")
    t.successful should be (true)
    t.get should be (-11)
  }
}
