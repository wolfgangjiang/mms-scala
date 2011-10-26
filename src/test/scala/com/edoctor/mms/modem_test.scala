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
          val ppp_id_counter = new PppIdCounter(0x10)
          val lcp_automaton = new LcpAutomaton(duplex, ppp_id_counter)
          lcp_automaton.state should be (LcpState.Closed)
          lcp_automaton.open
          lcp_automaton.state should be (LcpState.Ready)
          val pap_automaton = new PapAutomaton(duplex, ppp_id_counter)
          pap_automaton.authenticate
          val ipcp_automaton = new IpcpAutomaton(duplex, ppp_id_counter)
          val ip_list : List[Byte] = ipcp_automaton.get_ip 
          ip_list should have length (4)
          println("My Ip Address: " + ip_to_string(ip_list))
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

class MockDuplex extends AbstractDuplex with ShouldMatchers{
  val AT_chat_map = Map("ATZ" -> "OK",
                        "AT+IPR?" -> "+IPR: 115200",
                        "ATD*99***1#" -> "CONNECT 115200\n ~")

  private var text_response = ""

  val should_receive_list = List[String](
    "FF 03 C0 21 01 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 26 B4",
    "FF 03 C0 21 02 11 00 0A 02 06 00 00 00 00 A5 F0",
    "FF 03 C0 23 02 12 00 0D 08 57 65 6C 63 6F 6D 65 21 32 CD",
    "FF 03 80 21 01 01 00 0A 03 06 C0 A8 6F 6F B3 A7",
    "FF 03 80 21 03 13 00 0A 03 06 0A B8 EB F2 B1 5D",
    "FF 03 80 21 02 14 00 0A 03 06 0A B8 EB F2 74 98",
    "FF 03 C0 21 06 15 00 04 04 04")

  val should_send_list = List[String](
    "FF 03 C0 21 01 11 00 0A 02 06 00 00 00 00 CC 84",
    "FF 03 C0 21 02 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23 D0 47",    
    "FF 03 C0 23 01 12 00 06 00 00 0D 80",
    "FF 03 80 21 01 13 00 0A 03 06 00 00 00 00 7D 4C",
    "FF 03 80 21 02 01 00 0A 03 06 C0 A8 6F 6F DA D3",
    "FF 03 80 21 01 14 00 0A 03 06 0A B8 EB F2 1D EC",
    "FF 03 C0 21 05 15 00 04 C9 21")

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
    if(should_send_pointer >= should_send_list.length)
      fail("attempted too many sends")
    else {
      val bytes = should_send_list(should_send_pointer)
      should_send_pointer += 1
      expect(bytes) {
        to_hex_string(decode_0x7d_escape(frame.bytes.init.tail))
      }
    }
  }
}
