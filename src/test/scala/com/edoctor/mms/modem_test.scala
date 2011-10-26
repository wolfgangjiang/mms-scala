package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

abstract class AbstractModemSpec extends Spec
with ShouldMatchers with AbstractRemote

abstract class RemoteSanitySpec extends AbstractModemSpec {
  private var duplex : AbstractDuplex = null
  private var the_recent_packet : List[Byte] = null
  

  it("整体运行良好") {
    with_telnet_connection("192.168.10.243", 961) {
      dpx => {
        duplex = dpx
        test_dial
        test_first_incoming_lcp_packet
      }
    }
  }

  private def recent_packet = the_recent_packet

  private def test_dial : Unit = {
    duplex.say_text("ATD*99***1#")
    val response = duplex.listen_text(500L)
    response should include ("CONNECT")
    response should include ("~") // 0x7e, PPP数据包的边界标识字符    
  }

  private def test_first_incoming_lcp_packet : Unit = {
    read_next_packet
    recent_packet(0) should be(0xFF.toByte)
    recent_packet.length should be(48)
  }

  protected def read_next_packet : Unit = {
    the_recent_packet = duplex.read_binary(5*1000L)
  }
}

@RunWith(classOf[JUnitRunner])
class RemoteSanitySpecWithActual extends RemoteSanitySpec with ActualRemote

@RunWith(classOf[JUnitRunner])
class RemoteSanitySpecWithMock extends RemoteSanitySpec with MockRemote


