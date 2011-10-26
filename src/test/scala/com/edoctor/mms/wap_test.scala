package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import SessionHelpers._

@RunWith(classOf[JUnitRunner]) 
class IpDatagramSpecBasic extends Spec with ShouldMatchers {
  val example_bytes = parse_hex("45 00 02 0e fc 9b 40 00 fe 11 15 7a ac 15 00 04 c0 a8 fc 06") ++ parse_hex("DA DA")
  describe("ip datagram object") {
    it("unpacks a good datagram as a Right()") {
      val unpacked = IpDatagram.unpack(example_bytes)
      unpacked should be a ('right)
      unpacked.right.get should be (parse_hex("DA DA"))
    }
    it("unpacks a bad datagram as a Left()") {
      val bad_example = example_bytes.patch(5, parse_hex("00 00"), 2)
      val unpacked = IpDatagram.unpack(bad_example)
      unpacked should be a ('left)
    }
    it("provides checksum calculating outside parsing") {
      expect(true) {
        IpDatagram.is_header_checksum_good(example_bytes)
      }
    }
  }

  describe("ip datagram class") {
    val segment = new UdpDatagram(123, 321, parse_hex("DA DA"))
    val datagram = new IpDatagram(
      0x1234, parse_ip("10.123.221.2"), parse_ip("10.0.0.127"), 
      segment)
    it("can convert to bytes") {
      datagram.total_length should be (30)
      datagram.bytes should have length(30) // 20 + 8 + 2
    }
    it("has a correct checksum") {
      compute_checksum(datagram.bytes.take(20)) should be (0)
    }
  }
}

@RunWith(classOf[JUnitRunner]) 
class UdpDatagramSpecBasic extends Spec with ShouldMatchers {
  val source_ip_addr = parse_hex("ac 15 00 04")
  val destination_ip_addr = parse_hex("c0 a8 fc 06")
  describe("udp datagram object") {
    val example_bytes = parse_hex("00 43 00 44 01 fa 5d 4c 02 01 06 00 2e 21 0f 17 00 00 00 00 00 00 00 00 c0 a8 fc 06 ac 15 00 02 00 00 00 00 08 00 20 11 e0 1b 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 63 82 53 63 35 01 05 36 04 ac 15 00 04 01 04 ff ff ff 00 03 04 c0 a8 fc 01 1c 04 c0 a8 fc ff 40 09 64 68 63 70 2e 74 65 73 74 33 04 00 00 0e 10 02 04 ff ff c7 c0 04 04 ac 15 00 04 0f 10 73 6e 74 2e 65 61 73 74 2e 73 75 6e 2e 63 6f 6d 06 04 ac 15 00 01 0c 07 77 68 69 74 65 2d 36 2b a6 02 04 81 94 ae 1b 03 08 61 74 6c 61 6e 74 69 63 0a 04 81 94 ae 1b 0b 08 61 74 6c 61 6e 74 69 63 0f 05 78 74 65 72 6d 04 35 2f 65 78 70 6f 72 74 2f 73 32 38 2f 62 61 73 65 2e 73 32 38 73 5f 77 6f 73 2f 6c 61 74 65 73 74 2f 53 6f 6c 61 72 69 73 5f 38 2f 54 6f 6f 6c 73 2f 42 6f 6f 74 0c 20 2f 65 78 70 6f 72 74 2f 73 32 38 2f 62 61 73 65 2e 73 32 38 73 5f 77 6f 73 2f 6c 61 74 65 73 74 07 1b 2f 70 6c 61 74 66 6f 72 6d 2f 73 75 6e 34 6d 2f 6b 65 72 6e 65 6c 2f 75 6e 69 78 08 07 45 53 54 35 45 44 54 ff")

    it("unpacks a good datagram as a Right()") {
      val unpacked = UdpDatagram.unpack(
        example_bytes, source_ip_addr, destination_ip_addr)
      unpacked should be a ('right)
      unpacked.right.get should be (example_bytes.drop(8))
    }

    it("unpacks a bad datagram as a Left()") {
      val bad_example = 0xFF.toByte :: example_bytes.tail
      val unpacked = UdpDatagram.unpack(
        bad_example, source_ip_addr, destination_ip_addr)
      unpacked should be a ('left)
    }

    it("provides checksum calculating outside parsing") {
      expect(true) {
        UdpDatagram.is_checksum_good(
          example_bytes, source_ip_addr, destination_ip_addr)
      }
    }
  }

  describe("udp datagram class") {
    val datagram = new UdpDatagram(
      9201, 2048, parse_hex("DA DA"))
    
    it("can convert to bytes") {
      expect(10) {
        datagram.bytes(source_ip_addr,
                       destination_ip_addr).length
      }
    }
    it("has a correct checksum") {
      val pseudo_header = UdpDatagram.make_pseudo_header(
        source_ip_addr, destination_ip_addr, 10)
      expect(0) {
        compute_checksum(pseudo_header ++ datagram.bytes(
          source_ip_addr, destination_ip_addr))
      }
    }
    it("can be correctly unpacked back") {
      UdpDatagram.unpack(
        datagram.bytes(source_ip_addr, destination_ip_addr), 
        source_ip_addr, 
        destination_ip_addr) should be a ('right)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class WapSpecBasic extends Spec with ShouldMatchers {
  describe("WtpHeader") {
    it("is fine with typical connect invoke") {
      expect(parse_hex("0A 00 01 02")) {
        (new WtpHeader(WtpPduType.Invoke, 1, true, 2, 0)).bytes
      }
    }

    it("is fine with typical disconnect invoke") {
      expect(parse_hex("0A 00 01 00")) {
        (new WtpHeader(WtpPduType.Invoke, 1, true, 0, 0)).bytes
      }
    }

    it("is fine with typical segmented invoke") {
      expect(parse_hex("28 00 02 25")) {
        (new WtpHeader(WtpPduType.SegmentedInvoke, 2, false, 0, 0x25)).bytes
      }
    }

    it("can recognize pdu type from byte list") {
      expect(WtpPduType.Result) {
        WtpHeader.get_wtp_pdu_type(parse_hex("12 80 01"))
      }
    }
  }

  describe("uintvar converters") {
    import WspAutomaton._
    it("can convert int to uintvar") {      
      int_to_uintvar(40) should be (parse_hex("28"))
      int_to_uintvar(508) should be (parse_hex("83 7c"))
    }

    it("can convert uintvar to int") {
      uintvar_to_int(parse_hex("28")) should be (40)
      uintvar_to_int(parse_hex("83 7c")) should be (508)
    }
  }
}

//WspAutomaton should:
// send connect invoke and ack on connecting
// send disconnect invoke on disconnecting
// make correct segmentations on long data
// send segmentations in send_mms
// retransmit when timeout
// throw SessionTimeoutException if too many timeouts
// retransmit specified segment on remote nak
// throw SessionTimeoutException if cannot receive remote ack
@RunWith(classOf[JUnitRunner])
class WspAutomatonSpecBasic extends Spec with ShouldMatchers {
  def dummy_ip = parse_ip("1.1.1.1")

  def make_mock_duplex : UdpDuplex = {
    new UdpDuplex(new MockFrameDuplex(0x20),
                  dummy_ip, 1,
                  dummy_ip, 1)
  }

  def get_ppp_mock(duplex : UdpDuplex) : MockFrameDuplex = {
    duplex.ppp_duplex.asInstanceOf[MockFrameDuplex]
  }

  def extract_wtp(frame : PppFrame) : List[Byte] = {
    frame.protocol should be (Protocol.IP)
    val ip_data = frame.payload.asInstanceOf[IpData].bytes
    val data_inside_ip = IpDatagram.unpack(ip_data)
    data_inside_ip should be a ('right)
    val data_inside_udp = 
      UdpDatagram.unpack(data_inside_ip.right.get, dummy_ip, dummy_ip)
    data_inside_udp should be a ('right)
    data_inside_udp.right.get
  }

  describe("when connecting") {
    it("sends a connect invoke when told to open connection") {
      val mock_duplex = make_mock_duplex

      get_ppp_mock(mock_duplex).check( frame => {
        val data = extract_wtp(frame)
        WtpHeader.get_wtp_pdu_type(data) should be (WtpPduType.Invoke)
        WspPduType.get_wsp_pdu_type(data) should be (WspPduType.Connect)
      })

      val wsp_automaton = new WspAutomaton(mock_duplex)
      try { wsp_automaton.connect } 
      catch { case e : SessionTimeoutException => }

      get_ppp_mock(mock_duplex) should be ('satisfied)
    }

    it("sends an ack when received connectReply and returns as success") {
      val mock_duplex = make_mock_duplex

      val dummy_connect_reply = parse_hex("12 80 15 02 00 00")
      val udp_datagram = new UdpDatagram(1, 1, dummy_connect_reply)
      val ip_datagram = 
        new IpDatagram(123, dummy_ip, dummy_ip, udp_datagram)
      val ip_data = new IpData(ip_datagram.bytes)

      get_ppp_mock(mock_duplex).produce(new PppFrame(Protocol.IP, ip_data))
      get_ppp_mock(mock_duplex).check( frame => { } ) // connect invoke
      get_ppp_mock(mock_duplex).check( frame => {
        val data = extract_wtp(frame)
        WtpHeader.get_wtp_pdu_type(data) should be (WtpPduType.Ack)
        WtpHeader.get_tid(data) should be (0x15)
      })

      val wsp_automaton = new WspAutomaton(mock_duplex)
      wsp_automaton.connect 
      wsp_automaton.state should be (WspState.Connected)

      get_ppp_mock(mock_duplex) should be ('satisfied)
    }
  }

  describe("when told to disconnect") {
    it("sends a disconnect invoke and returns as Closed") {
      val mock_duplex = make_mock_duplex

      get_ppp_mock(mock_duplex).check( frame => {
        val data = extract_wtp(frame)
        WtpHeader.get_wtp_pdu_type(data) should be (WtpPduType.Invoke)
        WspPduType.get_wsp_pdu_type(data) should be (WspPduType.Disconnect)
      })

      val wsp_automaton = new WspAutomaton(mock_duplex)
      wsp_automaton.set_state(WspState.Connected)
      wsp_automaton.disconnect 
      wsp_automaton.state should be (WspState.Closed)

      get_ppp_mock(mock_duplex) should be ('satisfied)
    }    
  }

  describe("when timeout") {
    it("retransmits when timeout") {
      val mock_duplex = make_mock_duplex
      val ppp_mock_duplex = get_ppp_mock(mock_duplex)

      val dummy_connect_reply = parse_hex("12 80 01 02 00 00")
      val udp_datagram = new UdpDatagram(1, 1, dummy_connect_reply)
      val ip_datagram = 
        new IpDatagram(123, dummy_ip, dummy_ip, udp_datagram)
      val ip_data = new IpData(ip_datagram.bytes)

      ppp_mock_duplex.produce(ppp_mock_duplex.default_timeout_frame)
      ppp_mock_duplex.produce(ppp_mock_duplex.default_timeout_frame)
      ppp_mock_duplex.produce(new PppFrame(Protocol.IP, ip_data))
      (1 to 3).foreach( _ =>
        ppp_mock_duplex.check( frame => { 
          val data = extract_wtp(frame)
          WtpHeader.get_wtp_pdu_type(data) should be (WtpPduType.Invoke)
        })) // connect invoke
      ppp_mock_duplex.check( frame => {
        val data = extract_wtp(frame)
        WtpHeader.get_wtp_pdu_type(data) should be (WtpPduType.Ack)
      })

      val wsp_automaton = new WspAutomaton(mock_duplex)
      wsp_automaton.connect 
      wsp_automaton.state should be (WspState.Connected)

      ppp_mock_duplex should be ('satisfied)
    }

    it("throws SessionTimeoutException when too many timeouts") {
      val mock_duplex = make_mock_duplex

      val wsp_automaton = new WspAutomaton(mock_duplex)
      intercept[SessionTimeoutException] {
        wsp_automaton.connect
      }
    }
  }
}



//Further : any unhandled packets and timeouts should be logged
//Further : wsp pdu types should get some symbolic representation
