package com.edoctor.mms

trait MockRemote extends AbstractRemote {
  def with_telnet_connection(modem_ip : String, modem_port : Int)
    (block : AbstractDuplex => Unit) : Unit = {
      block(new MockDuplex)
  }
}

class MockDuplex extends AbstractDuplex {
  def say_text(command : String) : Unit = {
    // do nothing, for now
  }

  def listen_text(timeout_millis : Long) : String = {
    "CONNECT 115200\n ~"
  }

  def read_binary(timeout_millis : Long) : List[Byte] = {
    List.fill(48)(0xFF.toByte)
  }
}
