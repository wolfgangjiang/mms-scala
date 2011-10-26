package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import SessionHelpers._

abstract class AbstractModemSpec extends Spec
with ShouldMatchers with AbstractRemote

abstract class RemoteSanitySpec extends AbstractModemSpec {
  private var duplex : AbstractDuplex = null
  private var the_recent_frame : PppFrame = null
  
  describe("remote") {
    // different tests should use different modem ports to 
    // be independent to each other
    it("is sane") {
      with_telnet_connection("192.168.10.243", 961) {
        dpx => {
          duplex = dpx
          test_ATZ
          test_AT_IPR
          test_dial
          test_first_incoming_lcp_packet
        }
      }
    }

    // different tests should use different modem ports to 
    // be independent to each other
    it("works well with local automaton") {
      with_telnet_connection("192.168.10.243", 962) {
        dpx => {
          duplex = dpx
          test_dial
          test_first_incoming_lcp_packet
        }
      }
    }
  }

  private def recent_frame = the_recent_frame

  private def test_ATZ : Unit = {
    duplex.say_text("ATZ")
    val response = duplex.listen_text(300L)
    response should include ("OK")
  }

  private def test_AT_IPR : Unit = {
    duplex.say_text("AT+IPR?")
    val response = duplex.listen_text(300L)
    response should include ("115200")
  }

  private def test_dial : Unit = {
    duplex.say_text("ATD*99***1#")
    val response = duplex.listen_text(500L)
    response should include ("CONNECT")
    response should include ("~") // 0x7e, PPP数据包的边界标识字符    
  }

  private def test_first_incoming_lcp_packet : Unit = {
    read_next_frame

    recent_frame.protocol should be(Protocol.LCP)
    val lcp_packet = recent_frame.payload.asInstanceOf[LcpPacket]
    lcp_packet.code should be(LcpCode.ConfigureRequest)
    lcp_packet.identifier should be(1)
    lcp_packet.data.head should be(1)
    lcp_packet.data.last should be(0x23)    
  }

  protected def read_next_frame : Unit = {
    the_recent_frame = duplex.read_ppp(5*1000L)
  }
}

@RunWith(classOf[JUnitRunner])
class RemoteSanitySpecWithActual extends RemoteSanitySpec with ActualRemote

@RunWith(classOf[JUnitRunner])
class RemoteSanitySpecWithMock extends RemoteSanitySpec with MockRemote



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

  private var text_response = ""
  

  def say_text(command : String) : Unit = {
    text_response = AT_chat_map.getOrElse(command, "ERROR")
  }

  def listen_text(timeout_millis : Long) : String = {
    val last_response = text_response
    text_response = ""
    last_response
  }

  def read_ppp(timeout_millis : Long) : PppFrame = {
    val raw_frame = parse_hex("FF 03 C0 21 01 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 26 B4")

    PppFrame.parse(encode_0x7d_escape(raw_frame))
  }

  def write_ppp(frame : PppFrame) : Unit = { }
}
