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


// send correct req and ack on opening connection
// send correct req and ack on closing connection
// send correct retransmit package when timeout
// throw correct exception when too many timeouts
// respond correctly to terminate request
// show correct state when asked whether is ready or other 

abstract class MockBinaryDuplex extends AbstractDuplex 
with org.scalatest.Assertions {
  def say_text(command : String) : Unit = { 
    fail("should not call say_text()")
  }

  def listen_text(timeout_millis : Long) : String = {
    fail("should not call listen_text()")
  }

  def read_ppp(timeout_millis : Long) : PppFrame = {
    fail("should not call read_ppp()")
  }

  def write_ppp(frame : PppFrame) : Unit = {
    fail("should not call write_ppp()")
  }
}

@RunWith(classOf[JUnitRunner])
class LcpAutomatonSpecBasic extends Spec with ShouldMatchers {
  val default_timeout_frame = new PppFrame(
    Protocol.Timeout, new ErrorMessage("default timeout frame in mock"))
  val test_config_req_options = parse_hex("03 04 C0 23")


  describe("chat correctly on opening connection") {
    it("sends a config-req when told to open") {
      val mock_duplex = new MockBinaryDuplex {
        override def read_ppp(timeout_millis : Long) : PppFrame = {
          default_timeout_frame
        }

        override def write_ppp(frame : PppFrame) : Unit = {
          frame.protocol should be(Protocol.LCP)
          val packet = frame.payload.asInstanceOf[LcpPacket]
          packet.code should be (LcpCode.ConfigureRequest)
          packet.data should be (
            SessionParameters.our_lcp_config_req_options)
        }
      }

      val automaton = new LcpAutomaton(mock_duplex)
      automaton.open
    }

    it("sends a corresponding config-ack if receives a config-req") {
      val mock_duplex = new MockBinaryDuplex {
        private var time_to_call_write_ppp = 0
        
        override def read_ppp(timeout_millis : Long) : PppFrame = {
          val config_req = new LcpPacket(
            LcpCode.ConfigureRequest, 0x99.toByte, test_config_req_options)
          new PppFrame(Protocol.LCP, config_req)
        }
        
        // first get a config-req and then a config-ack
        override def write_ppp(frame : PppFrame) : Unit = {
          time_to_call_write_ppp += 1
          frame.protocol should be (Protocol.LCP)
          val packet = frame.payload.asInstanceOf[LcpPacket]
          if(time_to_call_write_ppp == 1)
            packet.code should be (LcpCode.ConfigureRequest)
          else if(time_to_call_write_ppp == 2) {
            packet.code should be (LcpCode.ConfigureAck)
            packet.identifier should be (0x99.toByte)
            packet.data should be (test_config_req_options)
          }
          // more incoming packets may be retransmit and are neglected
        }
      }
      
      val automaton = new LcpAutomaton(mock_duplex)
      automaton.open
    }

    it("gets ready when received a config-req and then a config-ack") {
      val mock_duplex = new MockBinaryDuplex {
        private var time_to_call_read_ppp = 0
        private var incoming_config_req : LcpPacket = null

        override def read_ppp(timeout_millis : Long) : PppFrame = {
          time_to_call_read_ppp += 1
          val config_req = new LcpPacket(
            LcpCode.ConfigureRequest, 0x99.toByte, test_config_req_options)
          if(time_to_call_read_ppp == 1) 
            new PppFrame(Protocol.LCP, config_req)
          else if(time_to_call_read_ppp == 2) {
            if(incoming_config_req == null)
              fail("wanting to read Ack without sending Req")
            else {
              val config_ack = new LcpPacket(
                LcpCode.ConfigureAck, 
                incoming_config_req.identifier,
                incoming_config_req.data)
              new PppFrame(Protocol.LCP, config_ack)
            }
          }
          else
            default_timeout_frame
        }
        
        override def write_ppp(frame : PppFrame) : Unit = {
          frame.protocol should be (Protocol.LCP)
          val packet = frame.payload.asInstanceOf[LcpPacket]
          if(packet.code == LcpCode.ConfigureRequest)
            incoming_config_req = packet
        }
      }

      val automaton = new LcpAutomaton(mock_duplex)
      automaton.open
      automaton.state should be (LcpState.Ready)
    }

    it("gets ready when received a config-ack and then a config-req") {
      val mock_duplex = new MockBinaryDuplex {
        private var time_to_call_read_ppp = 0
        private var incoming_config_req : LcpPacket = null

        override def read_ppp(timeout_millis : Long) : PppFrame = {
          time_to_call_read_ppp += 1
          val config_req = new LcpPacket(
            LcpCode.ConfigureRequest, 0x99.toByte, test_config_req_options)
          if(time_to_call_read_ppp == 2) 
            new PppFrame(Protocol.LCP, config_req)
          else if(time_to_call_read_ppp == 1) {
            if(incoming_config_req == null)
              fail("wanting to read Ack without sending Req")
            else {
              val config_ack = new LcpPacket(
                LcpCode.ConfigureAck, 
                incoming_config_req.identifier,
                incoming_config_req.data)
              new PppFrame(Protocol.LCP, config_ack)
            }
          }
          else
            default_timeout_frame
        }
        
        override def write_ppp(frame : PppFrame) : Unit = {
          frame.protocol should be (Protocol.LCP)
          val packet = frame.payload.asInstanceOf[LcpPacket]
          if(packet.code == LcpCode.ConfigureRequest)
            incoming_config_req = packet
        }
      }

      val automaton = new LcpAutomaton(mock_duplex)
      automaton.open
      automaton.state should be (LcpState.Ready)
    }
  }

  describe("chat correctly on closing connection") {
    it("sends a terminate-req and is closed when get an ack") {
      val mock_duplex = new MockBinaryDuplex {
        private var time_to_call_read_ppp = 0
        private var incoming_terminate_req : LcpPacket = null

        override def read_ppp(timeout_millis : Long) : PppFrame = {
          if(incoming_terminate_req == null)
            fail("wanting to read Ack without sending Req")
          else {
            val terminate_ack = new LcpPacket(
              LcpCode.TerminateAck, 
              incoming_terminate_req.identifier,
              Nil)
            new PppFrame(Protocol.LCP, terminate_ack)
          }
        }

        override def write_ppp(frame : PppFrame) : Unit = {
          frame.protocol should be (Protocol.LCP)
          val packet = frame.payload.asInstanceOf[LcpPacket]
          packet.code should be (LcpCode.TerminateRequest)
          incoming_terminate_req = packet
        }
      }

      val automaton = new LcpAutomaton(mock_duplex)
      automaton.set_state(LcpState.Ready)
      automaton.close
      automaton.state should be (LcpState.Closed)
    }
  }
}

