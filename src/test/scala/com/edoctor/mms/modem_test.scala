package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import SessionHelpers._

abstract class AbstractModemSpec extends Spec
with ShouldMatchers with AbstractRemote

abstract class RemoteSpec extends AbstractModemSpec {
  private var duplex : AbstractDuplex = null
  private var the_recent_frame : PppFrame = null
  
  describe("remote") {
    // different tests should use different modem ports to 
    // be independent to each other
    it("works well with local automaton") {
      with_telnet_connection("192.168.10.243", 962) {
        dpx => {
          duplex = dpx
          test_dial
          val lcp_automaton = new LcpAutomaton(duplex)
          lcp_automaton.state should be (LcpState.Closed)
          lcp_automaton.open
          lcp_automaton.state should be (LcpState.Ready)
          val pap_automaton = new PapAutomaton(duplex)
          pap_automaton.authenticate
          val ipcp_automaton = new IpcpAutomaton(duplex)
          val our_ip_addr : List[Byte] = ipcp_automaton.get_ip 
          our_ip_addr should have length (4)
          println("My Ip Address: " + ip_to_string(our_ip_addr))
          val udp_duplex = new UdpDuplex(
            duplex,
            our_ip_addr, 
            SessionParameters.our_port,
            SessionParameters.wap_proxy_ip_addr, 
            SessionParameters.wap_proxy_port)
          val wsp_automaton = new WspAutomaton(udp_duplex)
          wsp_automaton.connect
          wsp_automaton.disconnect
          lcp_automaton.close
          lcp_automaton.state should be (LcpState.Closed)
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
}

@RunWith(classOf[JUnitRunner])
class RemoteSpecWithActual extends RemoteSpec with ActualRemote

@RunWith(classOf[JUnitRunner])
class RemoteSpecWithMock extends RemoteSpec with MockRemote



trait MockRemote extends AbstractRemote {
  def with_telnet_connection(modem_ip : String, modem_port : Int)
    (block : AbstractDuplex => Unit) : Unit = {
      block(new MockDuplex)
  }
}

class MockDuplex 
extends AbstractDuplex(SessionParameters.initial_ppp_id) 
with ShouldMatchers{
  val AT_chat_map = Map("ATZ" -> "OK",
                        "AT+IPR?" -> "+IPR: 115200",
                        "ATD*99***1#" -> "CONNECT 115200\n ~")

  private var text_response = ""



  val should_receive_list = List[String](
    "FF 03 C0 21 01 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 26 B4",
    "FF 03 C0 21 02 11 00 0A 02 06 00 00 00 00 A5 F0",
    "FF 03 C0 23 02 12 00 0D 08 57 65 6C 63 6F 6D 65 21 32 CD",
    "FF 03 80 21 01 01 00 0A 03 06 C0 A8 6F 6F B3 A7",
    "FF 03 80 21 03 13 00 0A 03 06 0A B1 F4 4F 98 B9",
    "FF 03 80 21 02 14 00 0A 03 06 0A B1 F4 4F 5D 7C",
    "FF 03 00 21 45 00 00 4C 50 EA 40 00 3A 11 E6 0A 0A 00 00 AC 0A B1 F4 4F 23 F1 08 00 00 38 50 95 12 80 01 02 88 D7 21 10 17 04 80 83 E0 00 04 81 86 A0 00 02 83 01 02 84 01 45 6E 63 6F 64 69 6E 67 2D 56 65 72 73 69 6F 6E 00 31 2E 32 00 80 90 A1 D6",
    "FF 03 C0 21 06 15 00 04 04 04")
    // "FF 03 C0 21 01 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 26 B4",
    // "FF 03 C0 21 02 11 00 0A 02 06 00 00 00 00 A5 F0",
    // "FF 03 C0 23 02 12 00 0D 08 57 65 6C 63 6F 6D 65 21 32 CD",
    // "FF 03 80 21 01 01 00 0A 03 06 C0 A8 6F 6F B3 A7",
    // "FF 03 80 21 03 13 00 0A 03 06 0A B8 EB F2 B1 5D",
    // "FF 03 80 21 02 14 00 0A 03 06 0A B8 EB F2 74 98",
    // "FF 03 C0 21 06 15 00 04 04 04")

  val should_send_list = List[String](
    "FF 03 C0 21 01 11 00 0A 02 06 00 00 00 00 CC 84",
    "FF 03 C0 21 02 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 D0 47",
    "FF 03 C0 23 01 12 00 06 00 00 0D 80",
    "FF 03 80 21 01 13 00 0A 03 06 00 00 00 00 7D 4C",
    "FF 03 80 21 02 01 00 0A 03 06 C0 A8 6F 6F DA D3",
    "FF 03 80 21 01 14 00 0A 03 06 0A B1 F4 4F 34 08",
    "FF 03 00 21 45 00 00 2E 23 46 00 00 FF 11 8E CC 0A B1 F4 4F 0A 00 00 AC 08 00 23 F1 00 1A 0A 1F 0A 00 01 02 01 10 0A 00 04 80 83 E0 00 04 81 86 A0 00 1B 47",
    "FF 03 00 21 45 00 00 1F 23 47 00 00 FF 11 8E DA 0A B1 F4 4F 0A 00 00 AC 08 00 23 F1 00 0B AF 3A 1A 00 01 F6 2D",
    "FF 03 00 21 45 00 00 24 23 48 00 00 FF 11 8E D4 0A B1 F4 4F 0A 00 00 AC 08 00 23 F1 00 10 E1 86 0A 00 02 00 05 88 D7 21 71 00",
    "FF 03 C0 21 05 15 00 04 C9 21")

    // "FF 03 C0 21 01 11 00 0A 02 06 00 00 00 00 CC 84",
    // "FF 03 C0 21 02 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 D0 47",    
    // "FF 03 C0 23 01 12 00 06 00 00 0D 80",
    // "FF 03 80 21 01 13 00 0A 03 06 00 00 00 00 7D 4C",
    // "FF 03 80 21 02 01 00 0A 03 06 C0 A8 6F 6F DA D3",
    // "FF 03 80 21 01 14 00 0A 03 06 0A B8 EB F2 1D EC",
    // "FF 03 C0 21 05 15 00 04 C9 21")




  var should_receive_pointer : Int = 0
  var should_send_pointer : Int = 0

  def say_text(command : String) : Unit = {
    text_response = AT_chat_map.getOrElse(command, "ERROR")
  }

  def listen_text(timeout_millis : Long) : String = {
    val last_response = text_response
    text_response = ""
    last_response
  }

  def read_ppp(timeout_millis : Long) : PppFrame = {
    if(should_receive_pointer >= should_receive_list.length)
      fail("attempted too many receives")
    else {
      val bytes = should_receive_list(should_receive_pointer)
      should_receive_pointer += 1
      PppFrame.parse(encode_0x7d_escape(parse_hex(bytes)))
    }
  }

  def write_ppp(frame : PppFrame) : Unit = { 
    // each time ip datagram identifier is randomly generated
    // and is unimportant in testing
    def clear_ip_id(bytes : List[Byte]) : Unit = {
      if(bytes.take(6) == parse_hex("FF 03 00 21 45 00")) // ip datagram
        bytes.patch(8, parse_hex("00 00"), 2)
      else
        bytes
    }

    if(should_send_pointer >= should_send_list.length)
      fail("attempted too many sends")
    else {
      val bytes = should_send_list(should_send_pointer)
      should_send_pointer += 1
      expect(clear_ip_id(parse_hex(bytes))) {
        clear_ip_id(decode_0x7d_escape(frame.bytes.init.tail))
      }
    }
  }
}
