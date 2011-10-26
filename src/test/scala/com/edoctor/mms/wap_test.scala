package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import SessionHelpers._

@RunWith(classOf[JUnitRunner]) 
class IpDatagramSpecBasic extends Spec with ShouldMatchers {
  val example_bytes = parse_hex("45 00 00 6c 92 cc 00 00 38 06 e4 04 92 95 ba 14 a9 7c 15 95") ++ List.fill(0x6c - 20)(0xAB.toByte)
  describe("ip datagram object") {
    it("can parse a datagram without checking checksum") {
      val datagram = IpDatagram.parse(example_bytes)
      datagram.source_ip_addr should be (parse_hex("92 95 ba 14"))
      datagram.destination_ip_addr should be (parse_hex("a9 7c 15 95"))
      datagram.data should be (List.fill(0x6c - 20)(0xAB.toByte))
      datagram.total_length should be (0x6c)
    }
    it("provides checksum calculating outside parsing") {
      expect(true) {
        IpDatagram.is_header_checksum_good(example_bytes)
      }
    }
  }

  describe("ip datagram class") {
    val datagram = new IpDatagram(
      0x1234, parse_ip("10.123.221.2"), parse_ip("10.0.0.127"), 
      parse_hex("DA DA"))
    it("can convert to bytes") {
      datagram.total_length should be (22)
      datagram.bytes should have length(22) // 20 + 2
    }
    it("has a correct checksum") {
      compute_checksum(datagram.bytes.take(20)) should be (0)
    }
  }
}
