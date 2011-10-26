package com.edoctor.mms

import SessionHelpers._

trait MockRemote extends AbstractRemote {
  def with_telnet_connection(modem_ip : String, modem_port : Int)
    (block : AbstractDuplex => Unit) : Unit = {
      block(new MockDuplex)
  }
}

class MockDuplex extends AbstractDuplex {
  val AT_chat_map = Map("ATZ" -> "OK",
                        "AT+IPR?" -> "+IPR: 115200",
                        "ATD*99***1#" -> "CONNECT 115200\n ~")

  private var text_response = "ERROR"
  

  def say_text(command : String) : Unit = {
    text_response = AT_chat_map.getOrElse(command, "ERROR")
  }

  def listen_text(timeout_millis : Long) : String = {
    text_response
  }

  def read_binary(timeout_millis : Long) : List[Byte] = {
    parse_hex("FF 03 C0 21 01 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 26 B4")
  }
}
