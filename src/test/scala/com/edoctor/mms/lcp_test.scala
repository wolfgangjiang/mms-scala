package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import SessionHelpers._

@RunWith(classOf[JUnitRunner]) 
class PppFrameSpecBasic extends Spec with ShouldMatchers {
  val config_req = parse_hex("01 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23")
  val raw_bytes = parse_hex("7E FF 7D 23 C0 21 7D 21 7D 21 7D 20 7D 36 7D 21 7D 24 7D 25 DC 7D 22 7D 26 7D 20 7D 20 7D 20 7D 20 7D 27 7D 22 7D 28 7D 22 7D 23 7D 24 C0 23 26 B4 7E")
  
  it("can convert to raw bytes") {
    val packet = LcpPacket.parse(config_req)
    expect(raw_bytes) {
      (new PppFrame(Protocol.LCP, packet)).bytes
    }
  }
  
  it("can convert and parse back") {
    val in_packet = LcpPacket.parse(config_req)
    // remove 0x7e
    val bytes = (new PppFrame(Protocol.LCP, in_packet)).bytes.init.tail   
    val out_frame = PppFrame.parse(decode_0x7d_escape(bytes))
    out_frame.protocol should be (Protocol.LCP)
    val out_packet = out_frame.payload.asInstanceOf[LcpPacket]
    out_packet.code should be (in_packet.code)
    out_packet.identifier should be (in_packet.identifier)
    out_packet.data should be (in_packet.data)
  }
}

class MockFrameDuplex extends AbstractDuplex with ShouldMatchers {
  def say_text(command : String) : Unit = { 
    fail("should not call say_text()")
  }

  def listen_text(timeout_millis : Long) : String = {
    fail("should not call listen_text()")
  }

  val default_timeout_frame = new PppFrame(
    Protocol.Timeout, new ErrorMessage("default timeout frame in mock"))

  private var inputs : List[PppFrame] = Nil
  private var expectations : List[MockFrameExpectation] = Nil
  private var read_count = 0
  private var write_count = 0

  override def read_ppp(timeout_millis : Long) : PppFrame = {
    val result = if(read_count < inputs.length)
      inputs(read_count)
    else
      default_timeout_frame
    
    read_count += 1

    result
  }

  override def write_ppp(frame : PppFrame) : Unit = {
    if(write_count < expectations.length)
      expectations(write_count).check(frame)
    // else do nothing
    write_count += 1
  }

  def produce(frame : PppFrame) : Unit = {
    inputs = inputs :+ frame
  }

  def check(block : PppFrame => Unit) : Unit = {
    val checker = new MockFrameExpectation { 
      def check(frame : PppFrame) : Unit = { block(frame) }
    }
    expectations = expectations :+ checker
  }
}
abstract class MockFrameExpectation { def check(frame : PppFrame) : Unit }



@RunWith(classOf[JUnitRunner])
class LcpPacketSpecBasic extends Spec with ShouldMatchers {
  val config_req = parse_hex("01 01 00 16 01 04 05 DC 02 06 00 00 00 00 07 02 08 02 03 04 C0 23")
  val terminate_req = parse_hex("05 11 00 06 00 00")

  describe("LcpPacket object") {
    it("can parse raw bytes in constructor") {
      val packet = LcpPacket.parse(config_req)
      packet.code should be (LcpCode.ConfigureRequest)
      packet.identifier should be (1)
      packet.length should be (config_req.length)
      packet.data should be (config_req.drop(4))
    }

    it("can convert to raw bytes") {
      val packet = LcpPacket.parse(config_req)
      packet.bytes should be (config_req)
    }

    it("can label unknown code as LcpCode.Unknown") {
      val packet = LcpPacket.parse(0x55.toByte :: config_req.tail)
      packet.code should be (LcpCode.Unknown)
    }
  }

  describe("LcpPacket class") {
    it("can compose a packet and get raw bytes") {
      expect(terminate_req) {
        (new LcpPacket(
          LcpCode.TerminateRequest, 0x11.toByte, parse_hex("00 00"))).bytes
      }
    }

    it("can compose a packet") {
      val packet = new LcpPacket(
        LcpCode.TerminateRequest, 0x11.toByte, parse_hex("00 00"))
      packet.code should be (LcpCode.TerminateRequest)
      packet.identifier should be (0x11)
      packet.length should be (terminate_req.length)
      packet.data should be (parse_hex("00 00"))
    }
  }
}


//LcpAutomaton should:
// send correct req and ack on opening connection
// send correct req and ack on closing connection
// send correct retransmit package when timeout
// throw correct exception when too many timeouts
// respond correctly to terminate request
// show correct state when asked whether is ready or other 
@RunWith(classOf[JUnitRunner])
class LcpAutomatonSpecBasic extends Spec with ShouldMatchers {
  val default_timeout_frame = new PppFrame(
    Protocol.Timeout, new ErrorMessage("default timeout frame in mock"))
  val test_config_req_options = parse_hex("03 04 C0 23")

  private def extract_lcp(frame : PppFrame) : LcpPacket = {
    frame.protocol should be (Protocol.LCP)
    frame.payload.asInstanceOf[LcpPacket]
  }

  describe("on opening connection") {
    it("sends a config-req when told to open") {
      val mock_duplex = new MockFrameDuplex 

      mock_duplex.check(frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.ConfigureRequest)
        packet.identifier should be (0x21)
        packet.data should be (
          SessionParameters.our_lcp_config_req_options)
      })        

      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      intercept[SessionTimeoutException] {
        automaton.open
      }
    }

    it("sends a corresponding config-ack if receives a config-req") {
      val config_req = new LcpPacket(
        LcpCode.ConfigureRequest, 0x99.toByte, test_config_req_options)

      val mock_duplex = new MockFrameDuplex 

      mock_duplex.produce(new PppFrame(Protocol.LCP, config_req))
      mock_duplex.check(frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.ConfigureRequest)
      })
      mock_duplex.check(frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.ConfigureAck)
        packet.identifier should be (0x99.toByte)
        packet.data should be (test_config_req_options)
      })

      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      intercept[SessionTimeoutException] {
        automaton.open
      }
    }

    it("gets ready when received a config-req and then a config-ack") {
      val config_req = new LcpPacket(
        LcpCode.ConfigureRequest, 0x01.toByte, test_config_req_options)
      val config_ack = new LcpPacket(
        LcpCode.ConfigureAck, 0x11.toByte, Nil)
      val mock_duplex = new MockFrameDuplex

      mock_duplex.produce(new PppFrame(Protocol.LCP, config_req))
      mock_duplex.produce(new PppFrame(Protocol.LCP, config_ack))

      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      automaton.open
      automaton.state should be (LcpState.Ready)
    }

    it("gets ready when received a config-ack and then a config-req") {
      val config_req = new LcpPacket(
        LcpCode.ConfigureRequest, 0x01.toByte, test_config_req_options)
      val config_ack = new LcpPacket(
        LcpCode.ConfigureAck, 0x11.toByte, Nil)
      val mock_duplex = new MockFrameDuplex

      mock_duplex.produce(new PppFrame(Protocol.LCP, config_ack))
      mock_duplex.produce(new PppFrame(Protocol.LCP, config_req))

      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      automaton.open
      automaton.state should be (LcpState.Ready)
    }
  }

  describe("on closing connection") {
    it("sends a terminate-req and is closed when get an ack") {
      val terminate_ack = new LcpPacket(
        LcpCode.TerminateAck, 0x99.toByte, Nil)
      val mock_duplex = new MockFrameDuplex
        
      mock_duplex.produce(new PppFrame(Protocol.LCP, terminate_ack))
      mock_duplex.check( frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.TerminateRequest)
      })

      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      automaton.set_state(LcpState.Ready)
      automaton.close
      automaton.state should be (LcpState.Closed)
    }
  }

  describe("on timeout") {
    it("retransmits when timeout") {
      val terminate_ack = new LcpPacket(
        LcpCode.TerminateAck, 0x23.toByte, Nil)
      val mock_duplex = new MockFrameDuplex

      mock_duplex.produce(mock_duplex.default_timeout_frame)
      mock_duplex.produce(mock_duplex.default_timeout_frame)
      mock_duplex.produce(new PppFrame(Protocol.LCP, terminate_ack))
      mock_duplex.check( frame => {
        val packet = extract_lcp(frame)
        packet.identifier should be (0x21)
      })
      mock_duplex.check( frame => {
        val packet = extract_lcp(frame)
        packet.identifier should be (0x22)
      })
      mock_duplex.check( frame => {
        val packet = extract_lcp(frame)
        packet.identifier should be (0x23)
      })
      
      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      automaton.set_state(LcpState.Ready)
      automaton.close
    }
  
    it("throws SessionTimeoutException when too many retransmits") {
      val mock_duplex = new MockFrameDuplex
    
      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      intercept[SessionTimeoutException] {
        automaton.open
      }
      id_counter.get_id should be (
        0x20 + SessionParameters.max_retransmit_times + 2)
      // "+2" is because there are two "get_id"s that are not retransmit
      // one is the first send, the other is here
    }
  }

  describe("on incoming terminate request") {
    it("sends a terminate ack and throws a ClosedByRemoteException") {
      val mock_duplex = new MockFrameDuplex
      val terminate_req = new LcpPacket(
        LcpCode.TerminateRequest, 0x99.toByte, Nil)
      
      mock_duplex.produce(new PppFrame(Protocol.LCP, terminate_req))
      mock_duplex.check( frame => { } ) // a dummy config-request
      mock_duplex.check( frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.TerminateAck)
        packet.identifier should be (0x99)
      })

      val id_counter = new PppIdCounter(0x20)
      val automaton = new LcpAutomaton(mock_duplex, id_counter)
      intercept[ClosedByRemoteException] {
        automaton.open
      }
    }
  }
}

//PapAutomaton should:
// send an Authenticate-Request when activated
// signal success if received Authenticate-Ack
// throw AuthenticateFailureException if received Authenticate-Nak
// retransmit if timeout
// deem it as timeout if only received unknown packets
// throw SessionTimeoutException if too many timeouts
// ignore other packets, silently discard them
