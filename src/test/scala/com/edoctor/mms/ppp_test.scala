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

class MockFrameDuplex(initial_id : Int) 
extends AbstractDuplex(initial_id) with ShouldMatchers {
  override def name = "MockFrame"

  private var i_am_satisfied = false
  def satisfied = (expectations.isEmpty || i_am_satisfied)

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
    if(write_count < expectations.length) {
      expectations(write_count).check(frame)
      write_count += 1
      if(write_count == expectations.length)
        i_am_satisfied = true
    }
    // else do nothing
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
    it("can parse raw bytes and return a packet object") {
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
      val mock_duplex = new MockFrameDuplex(0x20)

      mock_duplex.check(frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.ConfigureRequest)
        packet.identifier should be (0x21)
        packet.data should be (
          SessionParameters.our_lcp_config_req_options)
      })        

      val automaton = new LcpAutomaton(mock_duplex)
      intercept[SessionTimeoutException] {
        automaton.open
      }
      mock_duplex should be ('satisfied)
    }

    it("sends a corresponding config-ack if receives a config-req") {
      val config_req = new LcpPacket(
        LcpCode.ConfigureRequest, 0x99.toByte, test_config_req_options)

      val mock_duplex = new MockFrameDuplex(0x20)

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

      val automaton = new LcpAutomaton(mock_duplex)
      intercept[SessionTimeoutException] {
        automaton.open
      }
      mock_duplex should be ('satisfied)
    }

    it("gets ready when received a config-req and then a config-ack") {
      val config_req = new LcpPacket(
        LcpCode.ConfigureRequest, 0x01.toByte, test_config_req_options)
      val config_ack = new LcpPacket(
        LcpCode.ConfigureAck, 0x11.toByte, Nil)
      val mock_duplex = new MockFrameDuplex(0x20)

      mock_duplex.produce(new PppFrame(Protocol.LCP, config_req))
      mock_duplex.produce(new PppFrame(Protocol.LCP, config_ack))

      val automaton = new LcpAutomaton(mock_duplex)
      automaton.open
      automaton.state should be (LcpState.Ready)
      mock_duplex should be ('satisfied)
    }

    it("gets ready when received a config-ack and then a config-req") {
      val config_req = new LcpPacket(
        LcpCode.ConfigureRequest, 0x01.toByte, test_config_req_options)
      val config_ack = new LcpPacket(
        LcpCode.ConfigureAck, 0x11.toByte, Nil)
      val mock_duplex = new MockFrameDuplex(0x20)

      mock_duplex.produce(new PppFrame(Protocol.LCP, config_ack))
      mock_duplex.produce(new PppFrame(Protocol.LCP, config_req))

      val automaton = new LcpAutomaton(mock_duplex)
      automaton.open
      automaton.state should be (LcpState.Ready)
      mock_duplex should be ('satisfied)
    }
  }

  describe("on closing connection") {
    it("sends a terminate-req and is closed when get an ack") {
      val terminate_ack = new LcpPacket(
        LcpCode.TerminateAck, 0x99.toByte, Nil)
      val mock_duplex = new MockFrameDuplex(0x20)
        
      mock_duplex.produce(new PppFrame(Protocol.LCP, terminate_ack))
      mock_duplex.check( frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.TerminateRequest)
      })

      val automaton = new LcpAutomaton(mock_duplex)
      automaton.set_state(LcpState.Ready)
      automaton.close
      automaton.state should be (LcpState.Closed)
      mock_duplex should be ('satisfied)
    }
  }

  describe("on timeout") {
    it("retransmits when timeout") {
      val terminate_ack = new LcpPacket(
        LcpCode.TerminateAck, 0x23.toByte, Nil)
      val mock_duplex = new MockFrameDuplex(0x20)

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
      
      val automaton = new LcpAutomaton(mock_duplex)
      automaton.set_state(LcpState.Ready)
      automaton.close
      mock_duplex should be ('satisfied)
    }
  
    it("throws SessionTimeoutException when too many retransmits") {
      val mock_duplex = new MockFrameDuplex(0x20)
    
      val automaton = new LcpAutomaton(mock_duplex)
      intercept[SessionTimeoutException] {
        automaton.open
      }
      mock_duplex should be ('satisfied)
      mock_duplex.get_ppp_id should be (
        0x20 + SessionParameters.max_retransmit_times + 2)
      // "+2" is because there are two "get_id"s that are not retransmit
      // one is the first send, the other is here
    }
  }

  describe("on incoming terminate request") {
    it("sends a terminate ack and throws a ClosedByRemoteException") {
      val mock_duplex = new MockFrameDuplex(0x20)
      val terminate_req = new LcpPacket(
        LcpCode.TerminateRequest, 0x99.toByte, Nil)
      
      mock_duplex.produce(new PppFrame(Protocol.LCP, terminate_req))
      mock_duplex.check( frame => { } ) // a dummy config-request
      mock_duplex.check( frame => {
        val packet = extract_lcp(frame)
        packet.code should be (LcpCode.TerminateAck)
        packet.identifier should be (0x99.toByte)
      })

      val automaton = new LcpAutomaton(mock_duplex)
      intercept[ClosedByRemoteException] {
        automaton.open
      }
      mock_duplex should be ('satisfied)
    }
  }
}

//PapPacket should:
// can parse raw bytes and return a packet object
// convert to raw bytes
// can label unknown code as PapCode.Unknown
// can compose a packet
// can compose a packet and get raw bytes
@RunWith(classOf[JUnitRunner])
class PapPacketSpecBasic extends Spec with ShouldMatchers {
  val auth_req = parse_hex("01 23 00 06 00 00")
  
  describe("PapPacket object") {
    it("can parse raw bytes and return a packet object") {
      val packet = PapPacket.parse(auth_req)
      packet.code should be (PapCode.AuthenticateRequest)
      packet.identifier should be (0x23)
      packet.length should be (auth_req.length)
      packet.data should be (auth_req.drop(4))
    }

    it("can convert to raw bytes") {
      val packet = PapPacket.parse(auth_req)
      packet.bytes should be (auth_req)
    }

    it("can label unknown code as PapCode.Unknown") {
      val packet = PapPacket.parse(0x55.toByte :: auth_req.tail)
      packet.code should be (PapCode.Unknown)
    }
  }

  describe("PapPacket class") {
    it("can compose a packet") {
      val packet = new PapPacket(
        PapCode.AuthenticateRequest, 0x23.toByte, parse_hex("00 00"))
      packet.code should be (PapCode.AuthenticateRequest)
      packet.identifier should be (0x23)
      packet.data should be (parse_hex("00 00"))
    }

    it("can compose a packet and get raw bytes") {
      expect(auth_req) {
        (new PapPacket(PapCode.AuthenticateRequest,
                       0x23.toByte, 
                       parse_hex("00 00"))).bytes
      }
    }
  }
}

//PapAutomaton should:
// send an Authenticate-Request when activated
// return without incident if received Authenticate-Ack
// throw AuthenticateFailureException if received Authenticate-Nak
// retransmit if timeout
// deem it as timeout if only received unknown packets but not proper ack
// throw SessionTimeoutException if too many timeouts
@RunWith(classOf[JUnitRunner])
class PapAutomatonSpecBasic extends Spec with ShouldMatchers {
  val auth_ack = {
    val message = "Welcome!"
    new PapPacket(
      PapCode.AuthenticateAck, 
      0x21.toByte, 
      message.length.toByte :: message.getBytes.toList)
  }

  val auth_nak = {
    val message = "Sorry! auth. failed"
    new PapPacket(
      PapCode.AuthenticateNak, 
      0x21.toByte, 
      message.length.toByte :: message.getBytes.toList)
  }

  private def extract_pap(frame : PppFrame) : PapPacket = {
    frame.protocol should be (Protocol.PAP)
    frame.payload.asInstanceOf[PapPacket]
  }

  it("sends an Authenticate-Request when activated") {
    val mock_duplex = new MockFrameDuplex(0x20)

    mock_duplex.check( frame => {
      val packet = extract_pap(frame)
      packet.code should be (PapCode.AuthenticateRequest)
    })

    val automaton = new PapAutomaton(mock_duplex)
    intercept[SessionTimeoutException] {
      automaton.authenticate
    }
    mock_duplex should be ('satisfied)
  }

  it("returns without incident if received Authenticate-Ack") {
    val mock_duplex = new MockFrameDuplex(0x20)

    mock_duplex.produce(new PppFrame(Protocol.PAP, auth_ack))

    val automaton = new PapAutomaton(mock_duplex)
    automaton.authenticate
    mock_duplex should be ('satisfied)
  }


  it("throws AuthenticateFailureException if received Authenticate-Nak") {
    val mock_duplex = new MockFrameDuplex(0x20)

    mock_duplex.produce(new PppFrame(Protocol.PAP, auth_nak))

    val automaton = new PapAutomaton(mock_duplex)
    intercept[AuthenticateFailureException] {
      automaton.authenticate
    }
    mock_duplex should be ('satisfied)
  }

  it("retransmits if timeout") {
    val mock_duplex = new MockFrameDuplex(0x1E)

    mock_duplex.produce(mock_duplex.default_timeout_frame)
    mock_duplex.produce(mock_duplex.default_timeout_frame)
    mock_duplex.produce(new PppFrame(Protocol.PAP, auth_ack))
    
    mock_duplex.check( frame => {
      val packet = extract_pap(frame)
      packet.identifier should be (0x1F)
    })
    mock_duplex.check( frame => {
      val packet = extract_pap(frame)
      packet.identifier should be (0x20)
    })
    mock_duplex.check( frame => {
      val packet = extract_pap(frame)
      packet.identifier should be (0x21)
    })

    val automaton = new PapAutomaton(mock_duplex)
    automaton.authenticate
    mock_duplex should be ('satisfied)
  }

  it("throws SessionTimeoutException if too many timeouts") {
    val mock_duplex = new MockFrameDuplex(0x20)

    val automaton = new PapAutomaton(mock_duplex)
    intercept[SessionTimeoutException] {
      automaton.authenticate
    }
    mock_duplex should be ('satisfied)    
  }

  it("deems as timeout if only received unknown packets but not proper ack") {
    val mock_duplex = new MockFrameDuplex(0x1E)

    // irrelevant LCP packets
    (1 to 4).foreach { _ =>
      mock_duplex.produce(new PppFrame(Protocol.LCP, auth_ack)) 
    }
    
    val automaton = new PapAutomaton(mock_duplex)
    intercept[SessionTimeoutException] {
      automaton.authenticate
    }
    mock_duplex should be ('satisfied)    
  }
}


//IpcpPacket should:
// can parse raw bytes and return a packet object
// convert to raw bytes
// can label unknown code as IpcpCode.Unknown
// can label malformed ip address as IpcpCode.Unknown
// can compose a packet
// can compose a packet and get raw bytes
@RunWith(classOf[JUnitRunner])
class IpcpPacketSpecBasic extends Spec with ShouldMatchers {
  val config_req = parse_hex("01 01 00 0A 03 06 C0 A8 6F 6F")
  val config_nak = parse_hex("03 01 00 0A 03 06 0A 12 34 56")

  describe("IpcpPacket object") {
    it("can parse raw bytes and return a packet object") {
      val packet = IpcpPacket.parse(config_req)
      packet.code should be (IpcpCode.ConfigureRequest)
      packet.identifier should be (0x01)
      packet.length should be (config_req.length)
      packet.ip_list should be (parse_hex("C0 A8 6F 6F"))
    }

    it("can convert to raw bytes") {
      val packet = IpcpPacket.parse(config_req)
      packet.bytes should be (config_req)
    }

    it("can label unknown code as IpcpCode.Unknown") {
      val packet = IpcpPacket.parse(0x55.toByte :: config_req.tail)
      packet.code should be (IpcpCode.Unknown)
    }

    it("can label malformed ip address as IpcpCode.Unknown") {
      val packet = IpcpPacket.parse(config_req.init)
      packet.code should be (IpcpCode.Unknown)
    }
  }

  describe("IpcpPacket class") {
    it("can compose a packet") {
      val packet = new IpcpPacket(
        IpcpCode.ConfigureNak, 0x01.toByte, parse_hex("0A 12 34 56"))
      packet.code should be (IpcpCode.ConfigureNak)
      packet.identifier should be (0x01.toByte)
      packet.ip_list should be (parse_hex("0A 12 34 56"))
    }

    it("can compose a packet and get raw bytes") {
      val packet = new IpcpPacket(
        IpcpCode.ConfigureNak, 0x01.toByte, parse_hex("0A 12 34 56"))
      packet.bytes should be (config_nak)
    }
  }
}

//IpcpAutomaton should:
// send a config-req with 0.0.0.0 when activated
// send a config-ack when received a config-req
// record ip and send a new config-req when received config-nak
// successfully return ip_list when received a config-ack
// retransmit when timeout
// throw SessionTimeoutException when too many timeouts
// silently discard unknown packets and malformed packets
// unsuccessful or unexpected interactions will result in timeout
@RunWith(classOf[JUnitRunner])
class IpcpAutomatonSpecBasic extends Spec with ShouldMatchers {
  val remote_config_req = new IpcpPacket(
    IpcpCode.ConfigureRequest, 0x01, parse_hex("12 34 AB CD"))
  val config_nak = new IpcpPacket(
    IpcpCode.ConfigureNak, 0x01, parse_hex("AB CD 43 21"))
  val config_ack = new IpcpPacket(
    IpcpCode.ConfigureAck, 0x01, parse_hex("AB CD 43 21"))

  private def extract_ipcp(frame : PppFrame) : IpcpPacket = {
    frame.protocol should be (Protocol.IPCP)
    frame.payload.asInstanceOf[IpcpPacket]
  }

  describe("on negotiation") {
    def check_happy_path(block : MockFrameDuplex => Unit) : Unit = {
      val mock_duplex = new MockFrameDuplex(0x20)
      
      block(mock_duplex)

      val automaton = new IpcpAutomaton(mock_duplex)
      try {
        automaton.get_ip
      } catch {
        case e : SessionTimeoutException => 
      }

      mock_duplex should be ('satisfied)      
    }
    
    it("sends a config-req with 0.0.0.0 when activated") {
      check_happy_path( mock_duplex => {
        mock_duplex.check( frame => {
          val packet = extract_ipcp(frame)
          packet.code should be (IpcpCode.ConfigureRequest)
          packet.ip_list should be (parse_hex("00 00 00 00"))
        })
      })
    }
    
    it("sends a config-ack when received a config-req") {
      check_happy_path(mock_duplex => {
        mock_duplex.produce(new PppFrame(Protocol.IPCP, remote_config_req))
        mock_duplex.check( frame => { } ) // a dummy config request
        mock_duplex.check( frame => {
          val packet = extract_ipcp(frame)
          packet.code should be (IpcpCode.ConfigureAck)
          packet.identifier should be (0x01)
          packet.ip_list should be (parse_hex("12 34 AB CD"))
        })
      })
    }

    it("record ip and send a new config-req when received config-nak") {
      check_happy_path(mock_duplex => {
        mock_duplex.produce(new PppFrame(Protocol.IPCP, config_nak))
        mock_duplex.check( frame => { } ) // a dummy config request
        mock_duplex.check( frame => {
          val packet = extract_ipcp(frame)
          packet.code should be (IpcpCode.ConfigureRequest)
          packet.identifier should not be (0x01)
          packet.ip_list should be (parse_hex("AB CD 43 21"))
        })
      })
    }

    it("successfully return ip_list when received a config-ack") {
      val mock_duplex = new MockFrameDuplex(0x20)
      
      mock_duplex.produce(new PppFrame(Protocol.IPCP, config_nak))
      mock_duplex.produce(new PppFrame(Protocol.IPCP, config_ack))

      val automaton = new IpcpAutomaton(mock_duplex)
      try {
        automaton.get_ip should be (config_nak.ip_list)
      } catch {
        case e : SessionTimeoutException => 
      }

      mock_duplex should be ('satisfied)      
    }
  }

  describe("on timeout") {
    it("retransmits if timeout") {
      val mock_duplex = new MockFrameDuplex(0x1E)

      mock_duplex.check( frame => {
        val packet = extract_ipcp(frame)
        packet.identifier should be (0x1F)
      })
      mock_duplex.check( frame => {
        val packet = extract_ipcp(frame)
        packet.identifier should be (0x20)
      })
      mock_duplex.check( frame => {
        val packet = extract_ipcp(frame)
        packet.identifier should be (0x21)
      })

      val automaton = new IpcpAutomaton(mock_duplex)
      try {
        automaton.get_ip
      } catch {
        case e : SessionTimeoutException =>
      }
      mock_duplex should be ('satisfied)
    }

    it("throw SessionTimeoutException if too many timeouts") {
      val mock_duplex = new MockFrameDuplex(0x20)

      val automaton = new IpcpAutomaton(mock_duplex)
      intercept[SessionTimeoutException] {
        automaton.get_ip
      }
      mock_duplex should be ('satisfied)    
    }

    it("deem as timeout if only received unknown packets") {
      val mock_duplex = new MockFrameDuplex(0x1E)

      // irrelevant LCP packets
      (1 to 4).foreach { _ =>
        mock_duplex.produce(new PppFrame(Protocol.LCP, config_nak))
      }
      
      val automaton = new IpcpAutomaton(mock_duplex)
      intercept[SessionTimeoutException] {
        automaton.get_ip
      }
      mock_duplex should be ('satisfied)    
    }
  }
}
