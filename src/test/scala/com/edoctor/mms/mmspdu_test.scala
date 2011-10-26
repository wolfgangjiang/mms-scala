package com.edoctor.mms

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import SessionHelpers._

object TestHelpers {
  def get_from_file(filename : String) : List[Byte] = {
    import java.io._
    val file = new File(filename)
    val len = file.length.toInt
    val fstream = new FileInputStream(file)
    val dstream = new DataInputStream(fstream)
    val byte_array = Array.ofDim[Byte](len)
    dstream.readFully(byte_array)
    byte_array.toList
  }

  def load_test_asset(filename : String) : List[Byte] = {
    get_from_file("test_assets/" + filename)
  }
}
import TestHelpers._

@RunWith(classOf[JUnitRunner])
class MmsPduSpecBasic extends Spec with ShouldMatchers {
  describe("tambur mms package") {
    import net.tambur.mms.{ MMMessage, MMConstants }
    val example_mms_bytes = parse_hex("8C 80 98 30 30 30 31 00 8D 81 89 11 80 2B 38 36 31 35 30 30 30 30 32 37 39 34 34 35 00 97 2B 38 36 31 33 31 32 32 37 34 37 36 30 35 2F 54 59 50 45 3D 50 4C 4D 4E 00 96 20 E5 BD A9 E4 BF A1 E6 B5 8B E8 AF 95 00 8A 80 8F 80 84 A3 01 04 0D 03 83 81 EA 20 E6 B5 8B E8 AF 95 E6 96 87 E6 9C AC")
    it("composes a mms message") {
      val msg = new MMMessage
      msg.setMessageType(MMConstants.MESSAGE_TYPE_M_SEND_REQ)
      msg.setTransactionId("0001")
      msg.setFrom("+86150000279445")
      msg.setTo("+86" + "13122747605" + "/TYPE=PLMN")
      msg.setSubject(" " + "彩信测试")
      msg.setVersion(1)
      msg.setContentType("application/vnd.wap.multipart.mixed")
      msg.addPart("text/plain; charset=\"utf-8\"", (" " + "测试文本").getBytes("utf-8"), false, null, null)
      val bytes : List[Byte] = msg.encode.toList

      bytes should be (example_mms_bytes)
    }
  }

  describe("MmsPdu basic header encoders") {
    import MmsPdu._
    it("encodes message type") {
      expect(parse_hex("8c 80")) {
        encode_message_type(MessageType.M_send_req)
      }
    }
    it("encodes transaction id") {
      expect(parse_hex("98 30 30 30 31 00 ")) {
        encode_transaction_id("0001")
      }
    }
    it("encodes version") {
      expect(parse_hex("8D 92")) {
        encode_version(1, 2)
      }
    }
    it("encodes sender (from)") {
      expect(parse_hex("89 10 80 2B 38 36 31 35 30 30 30 32 37 39 34 34 35 00")) {
        encode_sender("+8615000279445")
      }
    }
    it("encodes receiver (to)") {
      expect(parse_hex("97 2B 38 36 31 33 31 32 32 37 34 37 36 30 35 2F 54 59 50 45 3D 50 4C 4D 4E 00")) {
        encode_receiver("+8613122747605/TYPE=PLMN")
      }
    }
    it("encodes subject (title)") {
      expect(parse_hex("96 20 E5 BD A9 E4 BF A1 E6 B5 8B E8 AF 95 00")) {
        encode_subject("彩信测试")
      }
    }
    it("encodes multipart-related content type") {
      expect(parse_hex("84 1f 1f b3 8a 3c 62 69 67 2e 73 6d 69 6c 3e 00 89 61 70 70 6c 69 63 61 74 69 6f 6e 2f 73 6d 69 6c 00")) {
        encode_multipart_related("big.smil")
      }
      expect(parse_hex("84 1F 20 B3 8A 3C 73 75 64 6F 2E 73 6D 69 6C 3E 00 89 61 70 70 6C 69 63 61 74 69 6F 6E 2F 73 6D 69 6C 00")) {
        encode_multipart_related("sudo.smil")
      }
    }
    it("encodes multipart-mixed content type") {
      expect(parse_hex("84 A3")) {
        encode_multipart_mixed
      }
    }
    it("encodes message class as personal") {
      // 0x80 == personal, 0x81 == advertisement
      // 0x82 == informational, 0x83 == auto
      expect(parse_hex("8A 80")) {
        encode_message_class_as_personal
      }
    }
    it("encodes priority as normal") {
      // 0x80 == low, 0x81 == normal, 0x82 == high
      expect(parse_hex("8F 81")) {
        encode_priority_as_normal
      }
    }
  }

  describe("MmsPdu part encoders") {
    import MmsPdu._
    it("encodes text/plain in utf-8 with its name") {
      expect(parse_hex("0B 0D 03 83 81 EA 8E 6B 2E 74 78 74 00 20 E6 B5 8B E8 AF 95 E6 96 87 E6 9C AC")) {
        encode_part_text("测试文本".getBytes.toList, "k.txt")
      }
    }

    it("encodes smil file as index.smil") {
      expect(parse_hex("23 0D 13 61 70 70 6C 69 63 61 74 69 6F 6E 2F 73 6D 69 6C 00 81 83 C0 22 3C 69 6E 64 65 78 2E 73 6D 69 6C 3E 00 3C 73 6D 69 6C 3E 3C 2F 73 6D 69 6C 3E")) {
        encode_part_smil(("<smil></smil>").getBytes.toList)
      }
    }

    it("encodes image/gif with its name") {
      val gif_pic = load_test_asset("k.gif")
      gif_pic.take(3) should be ("GIF".getBytes.toList)
      gif_pic.length should be (45)

      val part_gif = encode_part_gif(gif_pic, "k.gif")
      part_gif.takeRight(gif_pic.length) should be (gif_pic)
      expect(parse_hex("10 2D 08 9D 85 6B 2E 67 69 66 00 8E 6B 2E 67 69 66 00")) {
        part_gif.dropRight(gif_pic.length)
      }      
    }

    it("encodes image/jpeg with its name") {
      val jpg_pic = load_test_asset("k.jpg")
      jpg_pic.take(4) should be (parse_hex("ff d8 ff e0"))
      jpg_pic.length should be (160)

      val part_jpg = encode_part_jpeg(jpg_pic, "k.jpg")
      part_jpg.takeRight(jpg_pic.length) should be (jpg_pic)
      expect(parse_hex("10 81 20 08 9E 85 6B 2E 6A 70 67 00 8E 6B 2E 6A 70 67 00")) {
        part_jpg.dropRight(jpg_pic.length)
      }
    }

    it("encodes image/png with its name") {
      val png_pic = load_test_asset("k.png")
      png_pic.take(8) should be (parse_hex("89 50 4E 47 0D 0A 1A 0A"))
      png_pic.length should be (284)

      val part_png = encode_part_png(png_pic, "k.png")
      part_png.takeRight(png_pic.length) should be (png_pic)
      expect(parse_hex("10 82 1C 08 A0 85 6B 2E 70 6E 67 00 8E 6B 2E 70 6E 67 00")) {
        part_png.dropRight(png_pic.length)
      }
    }

    it("encodes audio/mid with its name") {
      val mid = load_test_asset("requiem.mid")
      mid.take(8) should be (parse_hex("4d 54 68 64 00 00 00 06"))
      mid.length should be (12001)

      val part_mid = encode_part_midi(mid, "k.mid")
      part_mid.takeRight(mid.length) should be (mid)
      expect(parse_hex("19 DD 61 11 61 75 64 69 6F 2F 6D 69 64 00 85 6B 2E 6D 69 64 00 8E 6B 2E 6D 69 64 00")) {
        part_mid.dropRight(mid.length)
      }
    }

    it("composes a MmsRequest as bytes") {
      class MockMmsRequest(override val title : String,
                           override val to : String,
                           override val parts : Array[MmsRequestPart])
      extends MmsRequest

      class MockMmsRequestPart(override val name : String,
                               override val data_bytes : List[Byte])
      extends MmsRequestPart

      val part_0 = 
        new MockMmsRequestPart("k.jpg", parse_hex("AB CD EF"))
      val part_1 = 
        new MockMmsRequestPart("m.smil", parse_hex("AB CD EF"))
      val request = new MockMmsRequest(
        "mock测试", "13122747605", Array(part_0, part_1))

      val expected = parse_hex("8C 80 98 30 30 30 31 00 8D 91 89 1A 80 2B 38 36 31 35 30 30 30 32 37 39 34 34 35 2F 54 59 50 45 3D 50 4C 4D 4E 00 97 2B 38 36 31 33 31 32 32 37 34 37 36 30 35 2F 54 59 50 45 3D 50 4C 4D 4E 00 96 20 6D 6F 63 6B E6 B5 8B E8 AF 95 00 8A 80 8F 81 84 1F 21 B3 8A 3C 69 6E 64 65 78 2E 73 6D 69 6C 3E 00 89 61 70 70 6C 69 63 61 74 69 6F 6E 2F 73 6D 69 6C 00 02 10 03 08 9E 85 6B 2E 6A 70 67 00 8E 6B 2E 6A 70 67 00 AB CD EF 23 03 13 61 70 70 6C 69 63 61 74 69 6F 6E 2F 73 6D 69 6C 00 81 83 C0 22 3C 69 6E 64 65 78 2E 73 6D 69 6C 3E 00 AB CD EF")

      MmsPdu.compose(request) should be (expected)
    }
  }

  describe("json request parser") {
    import MmsPdu.parse_request
    it("uses google Gson library, which is reliable") {
      import com.google.gson.Gson
      val json = ("{ title : \"m\", to : \"o\", parts : " +
                  " [ {name : \"index.smil\", data_or_url : \"hey\" }, " +
                  "   {name : \"m.jpg\", data_or_url : \"link_to_jpg\" }]" +
                  "}")
      val obj = (new Gson).fromJson(json, classOf[MmsRequest])
      obj.title should be ("m")
      obj.to should be ("o")
      obj.parts.length should be (2)
      obj.parts(0).name should be ("index.smil")
      obj.parts(1).data_or_url should be ("link_to_jpg")
    }

    it("parses json request") {
      val json = (" { title : \"m\", to : \"13012344321\", parts : " +
                  " [ {name : \"index.smil\", data_or_url : \"hey\" }, " +
                  "   {name : \"m.jpg\", data_or_url : \"link_to_jpg\" }]" +
                  "} ")
      val either_obj = parse_request(json)
      either_obj should be a ('right)
      val obj = either_obj.right.get
      obj.date should be (null)
      obj.title should be ("m")
      obj.to should be ("13012344321")
      obj.parts.length should be (2)
      obj.parts(0).name should be ("index.smil")
      obj.parts(0).ext should be ("smil")
      obj.parts(1).data_or_url should be ("link_to_jpg")      
    }

    it("parses well when date is specified") {
      val json = (" { date: \"20110820\", " +
                  "title : \"m\", to : \"13012344321\", parts : " +
                  " [ {name : \"index.smil\", data_or_url : \"hey\" }, " +
                  "   {name : \"m.jpg\", data_or_url : \"link_to_jpg\" }]" +
                  "}")
      val either_obj = parse_request(json)
      either_obj should be a ('right)
      val obj = either_obj.right.get
      obj.date should be ("20110820")
    }

    it("correctly parses old format request in ver1.0") {
      val json = " [\"20110820\", \"13012344321\", \"标题\", \"内文\", \"pic_url.gif\", \"mid_url\"]"

      val either_obj = parse_request(json)
      either_obj should be a ('right)
      val obj = either_obj.right.get
      obj.date should be ("20110820")
      obj.to should be ("13012344321")
      obj.parts.length should be (4)
      obj.parts.last.name should be ("index.smil")
    }

    it("rejects malformed mobile number") {
      val json = ("{ title : \"m\", to : \"abcd\", parts : " +
                  " [ {name : \"index.smil\", data_or_url : \"hey\" }, " +
                  "   {name : \"m.jpg\", data_or_url : \"link_to_jpg\" }]" +
                  "}")
      val either_obj = parse_request(json)
      either_obj should be a ('left)      
    }

    it("rejects empty fields in parts") {
      val json = ("{ title : \"m\", to : \"13012344321\", parts : " +
                  " [ { } ]}")
      val either_obj = parse_request(json)
      either_obj should be a ('left)
      either_obj.left.get should be ("missing part name")
    }

    it("retrieves binary file from url") {
      val mock_http_server = new MockPicServer
      mock_http_server.start

      val json = ("{ title : \"m\", to : \"13012344321\", parts : " +
                  " [ { name : \"k.gif\", " +
                  " data_or_url : \"http://localhost:7000/k.gif\" } ] }")
      val either_obj = parse_request(json)
      either_obj should be a ('right)
      val obj = either_obj.right.get
      obj.parts(0).data_bytes.length should be (
        load_test_asset("k.gif").length)

      mock_http_server.stop
    }

    it("silently set empty data when fail to retrieve file from a url") {
      val json = ("{ title : \"m\", to : \"13012344321\", parts : " +
                  " [ { name : \"k.gif\", " +
                  " data_or_url : \"http://localhost:7000/k.gif\" } ] }")
      val either_obj = parse_request(json)
      either_obj should be a ('right)
      val obj = either_obj.right.get
      obj.parts(0).data_bytes.length should be (0)
    }

    describe("VersionUpgrader") {
      import MmsPdu.VersionUpgrader._

      def json_xml_escape(str : String) : String = 
        str.replace("=", "\\u003d").replace("<", "\\u003c").replace(">", "\\u003e")

      val typical_old_json = "[\"20110820\", \"13012344321\", \"标题\", \"内文\", \"pic_url.gif\", \"mid_url\"]"
      val typical_new_json = json_xml_escape(
        "{\"date\":\"20110820\",\"title\":\"标题\"," +
        "\"to\":\"13012344321\",\"parts\":[" +
        "{\"name\":\"k.gif\",\"data_or_url\":\"pic_url.gif\"}," +
        "{\"name\":\"k.txt\",\"data_or_url\":\"内文\"}," +
        "{\"name\":\"k.mid\",\"data_or_url\":\"mid_url\"}," +
        "{\"name\":\"index.smil\",\"data_or_url\":\"<smil><body><img src=\\\"k.gif\\\" /><text src=\\\"k.txt\\\" /><audio src=\\\"k.mid\\\" repeatCount=\\\"indefinite\\\" type=\\\"mid\\\" /></body></smil>\"}]}")

      it("transforms a 1.0 request to a 2.0 request") {
        val old_json = typical_old_json
        val new_json = typical_new_json
        upgrade_request_from_1_0(old_json) should be (new_json)
      }

      it("recognizes different pic url extension names") {
        val old_json = typical_old_json.replace("gif", "jpg")
        val new_json = typical_new_json.replace("gif", "jpg")
        upgrade_request_from_1_0(old_json) should be (new_json)
      }

      it("treats null url as non-request") {
        val old_json = typical_old_json.replace("\"mid_url\"", "null")
        val new_json = typical_new_json.
        replace("{\"name\":\"k.mid\",\"data_or_url\":\"mid_url\"},", "").
        replace(json_xml_escape("<audio src=\\\"k.mid\\\" repeatCount=\\\"indefinite\\\" type=\\\"mid\\\" />"), "")          

        upgrade_request_from_1_0(old_json) should be (new_json)
      }

      it("treats empty url as non-request") {
        val old_json = typical_old_json.replace("\"pic_url.gif\"", "\"\"")
        val new_json = typical_new_json.
        replace("{\"name\":\"k.gif\",\"data_or_url\":\"pic_url.gif\"},", "").
        replace(json_xml_escape("<img src=\\\"k.gif\\\" />"), "")

        upgrade_request_from_1_0(old_json) should be (new_json)
      }

      it("results in 2.0 MmsRequest object") {
        val either_request = 
          MmsPdu.parse_request(upgrade_request_from_1_0(typical_old_json))
        either_request should be a ('right)
        val request = either_request.right.get
        request.date should be ("20110820")
        request.title should be ("标题")
        request.parts.length should be (4)
        request.parts.head.name should be ("k.gif")
        request.parts.head.data_or_url should be ("pic_url.gif")
        request.parts.last.name should be ("index.smil")
      }

      it("treats \"none\" date as null") {
        val old_json = typical_old_json.replace("20110820", "none")
        val either_request = 
          MmsPdu.parse_request(upgrade_request_from_1_0(old_json))
        either_request should be a ('right)
        val request = either_request.right.get
        request.date should be (null)
      }
    }
  }  
}

class MockPicServer {
  import java.net.ServerSocket
  import java.io.DataOutputStream

  val image_data = load_test_asset("k.gif")
  val headers = List("HTTP/1.1 200 OK",
                     "Connection: close",
                     "Server: Mock",
                     "Content-Type: image/gif",
                     "Content-Length: " + image_data.length,
                     "").map(_ + "\r\n").mkString
                 
  val server_thread = new Thread() {
    override def run : Unit = {
      val server_socket = new ServerSocket(7000)
      val session_socket = server_socket.accept
      val out_stream = new DataOutputStream(session_socket.getOutputStream)
      out_stream.writeBytes(headers)
      out_stream.write(image_data.toArray)
      out_stream.flush
      out_stream.close
    }
  }

  def start = { server_thread.start }
  def stop = { }
}
