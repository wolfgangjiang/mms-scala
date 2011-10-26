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

@RunWith(classOf[JUnitRunner]) 
class UdpDatagramSpecBasic extends Spec with ShouldMatchers {
  val source_ip_addr = parse_ip("10.123.221.2")
  val destination_ip_addr = parse_ip("10.0.0.127")
  describe("udp datagram object") {
    val example_bytes = parse_hex("1A 1A 40 11 00 0A 00 00 DA DA")
    it("can parse a datagram without checking checksum") {
      val datagram = UdpDatagram.parse(example_bytes)
      datagram.source_port should be (0x1A1A)
      datagram.destination_port should be (0x4011)
      datagram.length should be (10)
      datagram.data should be (parse_hex("DA DA"))
    }
    it("provides checksum calculating outside parsing") {
      expect(false) {
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
  }
}
