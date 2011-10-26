package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class HelpersSpecBasic extends Spec with ShouldMatchers {
  import SessionHelpers._

  describe("decode_0x7d_escape") {
    it("works with test data") {
      expect(parse_hex("AB 00 7d 7e")) {
        decode_0x7d_escape(parse_hex("AB 7d 20 7d 5d 7d 5e"))
      }
    }

    it("will not crash when last byte is 0x7d") {
      expect(parse_hex("AB 00 7d 7e 7d")) {
        decode_0x7d_escape(parse_hex("AB 7d 20 7d 5d 7d 5e 7D"))
      }      
    }
  }

  describe("encode_0x7d_escape") {
    it("works with test data") {
      expect(parse_hex("AB 7d 20 7d 5d 7d 5e")) {
        encode_0x7d_escape(parse_hex("AB 00 7d 7e"))
      }
    }
  }

  describe("split_word") {
    it("works with two-byte int") {
      split_word(0xAB12) should be (parse_hex("AB 12"))
    }

    // 高2字节的数据应该被丢弃
    it("drops higher bytes") {      
      split_word(0x15CCAB12) should be (parse_hex("AB 12"))
    }
  }

  describe("split_double_word") {
    it("works with four-byte int") {
      split_double_word(0xC0831AA0) should be (parse_hex("C0 83 1A A0"))
    }
  }

  describe("byte_list_to_int") {
    it("converts a bytelist of four bytes to an int") {
      expect(0xB9105A50) {
        byte_list_to_int(parse_hex("B9 10 5A 50"))
      }
    }
    
    // 溢出的高位应该被丢弃
    it("drops left when get a list longer than four bytes") {
      expect(0x12345678) {
        byte_list_to_int(parse_hex("AB CD EF 12 34 56 78"))
      }
    }
    
    it("stuffs zero to left when get a list shorter than four bytes") {
      expect(0xABCD) {
        byte_list_to_int(parse_hex("AB CD"))
      }
    }
  }

  describe("to_hex_string") {
    it("convert list byte to readable string") {
      expect("AB 1A B5 09") {
        to_hex_string(List(0xAB, 0x1A, 0xB5, 0x09).map(_.toByte))
      }
    }
  }  

  describe("parse_hex") {
    it("converts printed string to binary data") {
      expect(parse_hex("AB 1A B5 09")) {
        List(0xAB, 0x1A, 0xB5, 0x09).map(_.toByte)
      }
    }
  }
}
