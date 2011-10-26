package com.edoctor.mms

import SessionHelpers._
import SessionParameters._
import Wsp.int_to_uintvar

object MmsPdu {
  sealed abstract class MessageType
  object MessageType {
    object M_send_req extends MessageType
  }

  def parse_request(json_request : String) : Either[String, MmsRequest] = {
    import com.google.gson.{ Gson, JsonParseException }
    def check_parts(request_obj : MmsRequest) : Either[String, MmsRequest] = {
      request_obj.parts.foreach(part => {
        if(part.name == null) return Left("missing part name")
        else if(part.data_or_url == null) return Left("missing data_or_url")
        // else pass check
      })
      return Right(request_obj)
    }

    try {
      val request_obj = 
        (new Gson).fromJson(json_request, classOf[MmsRequest])
      if(request_obj.title == null) Left("missing title")
      else if(request_obj.to == null) Left("missing destination")
      else if(!(request_obj.to matches """\d{4,}""")) Left("malformed mobile number")
      else if(request_obj.parts == null) Left("no any parts")
      else check_parts(request_obj)
    } catch {
      case e : JsonParseException =>
        Left("malformed json request : failed to parse")
    }
  }  

  def encode_zero_terminate_string(str : String) : List[Byte] = {
    str.getBytes.toList :+ 0x00.toByte
  }

  def encode_message_type(m_type : MessageType) : List[Byte] = {
    parse_hex("8c 80")
    // 0x8c == 0x80 + 0x0c == X-Mms-Message-Type
    // 0x80 means M_send_req, the only supported type for now
  }

  def encode_transaction_id(id : String) : List[Byte] = {
    0x98.toByte :: encode_zero_terminate_string(id)
    // 0x98 == 0x80 + 0x18 == X-Mms-Transaction-ID
  }

  def encode_version(major : Int, minor : Int) : List[Byte] = {
    val first_byte = 0x8D.toByte
    val second_byte = (major << 4 | minor | 0x80).toByte
    List[Byte](first_byte, second_byte)
    // 0x8D == 0x80 + 0x0D == X-Mms-Version
  }

  def encode_sender(mobile : String) : List[Byte] = {
    val first_byte = 0x89.toByte
    // 0x89 == 0x80 + 0x09 == From
    val length_byte = (mobile.length + 2).toByte
    val third_byte = 0x80.toByte 
    // this third byte is only require for sender, but not receiver
    List[Byte](first_byte, length_byte, third_byte) ++
    encode_zero_terminate_string(mobile)
  }

  def encode_receiver(mobile : String) : List[Byte] = {
    0x97.toByte :: encode_zero_terminate_string(mobile)
    // 0x97 == 0x80 + 0x17 == To
  }
  
  def encode_subject(subject : String) : List[Byte] = {
    0x96.toByte :: encode_zero_terminate_string(" " + subject)
    // 0x96 == 0x80 + 0x16 == Subject
    // a space is added, to correctly display Chinese character
    // this space char is a hack.
  }

  def encode_multipart_related(start : String) : List[Byte] = {
    val first_byte = 0x84.toByte // Content-Type
    val second_byte = 0x1f.toByte // indicate that a uintvar length follows
    val content_type_byte = 0xb3.toByte 
    // 0xb3 == 0x80 + 0x33 == application/vnd.wap.multipart.related
    val start_bytes = // the index (first) content of whole message
      0x8a.toByte :: encode_zero_terminate_string("<" + start + ">")
    // 0x8a == 0x80 + 0x0a == Wellknown Parameter: Start(for related only)
    // 0x8a may be deprecated and should use 0x99 instead
    val type_bytes = // type of start
      0x89.toByte :: encode_zero_terminate_string("application/smil")
    // 0x89 == 0x80 + 0x09 == Wellknown Parameter: Type(for related only)
    val length = start_bytes.length + type_bytes.length + 1
    // "+1" means the fourth_byte

    List(List(first_byte, second_byte),
         int_to_uintvar(length),
         List(content_type_byte),
         start_bytes,
         type_bytes).flatten
  }

  def encode_multipart_mixed : List[Byte] = {
    parse_hex("84 A3")
    // 0x84 == 0x80 + 0x04 == Content-Type
    // 0xA3 == 0x80 + 0x23 == application/vnd.wap.multipart.mixed
  }

  def encode_message_class_as_personal : List[Byte] = {
    parse_hex("8A 80")
    // 0x8a == 0x80 + 0x0a == Message-Class
    // second byte:
    // 0x80 == personal, 0x81 == advertisement
    // 0x82 == informational, 0x83 == auto
    // for now we only provide functionality of personal
  }

  def encode_priority_as_normal : List[Byte] = {
    parse_hex("8F 81")
    // 0x8F == 0x80 + 0x0F == Priority
    // second byte:
    // 0x80 == low, 0x81 == normal, 0x82 == high
    // for now we only provide functionality of normal priority
  }

  def encode_part_text(data : List[Byte]) : List[Byte] = {
    val header = parse_hex("03 83 81 EA") 
    // 0x03 == what? maybe content type length ?
    // 0x83 == 0x80 + 0x03 == "text/plain", 
    // 0x81 == 0x80 + 0x01 == charset indicator
    // 0xEA == 0x80 + 0x6A == utf-8 (well known charsets)
    val headers_len = int_to_uintvar(header.length)
    val data_len = int_to_uintvar(data.length)
    
    List(headers_len,
         data_len,
         header,
         data).flatten
  }
  
  def encode_part_smil(data : List[Byte]) : List[Byte] = {
    val content_type = encode_zero_terminate_string("application/smil")
    val additional_header = 
      0xC0.toByte :: encode_zero_terminate_string("\"<index.smil>")
    // 0xC0 == 0x80 + 0x40 == Content-ID
    // there should be only one quote as the first char, 
    // without closing quote
    val headers_len = int_to_uintvar(
      content_type.length + additional_header.length)
    val data_len = int_to_uintvar(data.length)

    List(headers_len, 
         data_len,
         content_type,
         additional_header,
         data).flatten
  }

  def encode_part_image(data : List[Byte], name : String,
                        content_type_byte : Byte) : List[Byte] = {
    val name_parameter = 0x85.toByte :: encode_zero_terminate_string(name)
    val content_type_len = name_parameter.length + 1
    // content_type.length == 1
    val headers_len = int_to_uintvar(content_type_len + 1)
    // headers_len = content_type_len + length(content_type_len_byte)
    val data_len = int_to_uintvar(data.length)

    List(headers_len,
         data_len,
         int_to_uintvar(content_type_len),
         List(content_type_byte),
         name_parameter,
         data).flatten
  }

  def encode_part_gif(data : List[Byte], name : String) : List[Byte] = {
    encode_part_image(data, name, 0x9d.toByte)
    // 0x9d == 0x80 + 0x1d == "image/gif"
  }

  def encode_part_jpeg(data : List[Byte], name : String) : List[Byte] = {
    encode_part_image(data, name, 0x9e.toByte)
    // 0x9d == 0x80 + 0x1e == "image/jpeg"
  }

  def encode_part_png(data : List[Byte], name : String) : List[Byte] = {
    encode_part_image(data, name, 0xA0.toByte)
    // 0xA0 == 0x80 + 0x20 == "image/png"
  }

  def encode_pdu_header(title : String, to : String) : List[Byte] = {
    List(encode_message_type(MessageType.M_send_req),
         encode_transaction_id("0001"),
         encode_version(1, 2),
         encode_sender("+8615000279445/TYPE=PLMN"),
         encode_receiver("+86" + to + "/TYPE=PLMN"),
         encode_subject(title),
         encode_message_class_as_personal,
         encode_priority_as_normal,
         encode_multipart_related("index.smil")).flatten
  }

  def encode_part(part : MmsRequestPart) : List[Byte] = {
      if(part.ext == "jpg" || part.ext == "jpeg")
        encode_part_jpeg(part.data_bytes, part.name)
      else if(part.ext == "gif")
        encode_part_gif(part.data_bytes, part.name)
      else if(part.ext == "png")
        encode_part_png(part.data_bytes, part.name)
      else if(part.ext == "smil")
        encode_part_smil(part.data_bytes)
      else
        encode_part_text(part.data_bytes)
  }

  def compose(request : MmsRequest) : List[Byte] = {
    val header_bytes = encode_pdu_header(request.title, request.to)
    val part_count_byte = List(request.parts.length.toByte)
    val part_bytes = request.parts.toList.map(encode_part).flatten
    
    List(header_bytes,
         part_count_byte,
         part_bytes).flatten
  }
}

class MmsRequestPart {
  val name : String = null
  val data_or_url : String = null
  private var the_data_bytes : List[Byte] = null

  def data_bytes : List[Byte] = {
    if(the_data_bytes == null)
      fill_data_bytes
    the_data_bytes
  }

  def ext = 
    if(data_or_url == null) ""
    else data_or_url.split("\\.").toList.last

  // 刚刚从json转化来的时候，the_data_bytes为空。
  // 如果是已知的二进制类型，则应该下载，认为是url，
  // 否则简单地按照字符串处理
  private def fill_data_bytes : Unit = {
    the_data_bytes = 
      if(List("jpg", "jpeg", "gif", "png", "mid", "midi") contains ext)
        download(data_or_url) // url
      else 
        data_or_url.getBytes.toList // string data
  }

  // 使用apache的httpclient从给定的url下载文件，将下载到的文件以二进制
  // 形式返回。它用来下载pic、midi等二进制文件。
  private def download(url : String) : List[Byte] = {
    import org.apache.commons.httpclient.{ HttpClient, HttpStatus }
    import org.apache.commons.httpclient.methods.GetMethod
    // Log.debug("download", "downloading " + url)

    val httpclient = new HttpClient()
    val httpget = new GetMethod(url)

    // 在这里必须手动设置timeout值，以毫秒为单位。如果不进行这样的设
    // 置，则对于响应慢的网站，可能导致整个彩信模块卡死在这里的等待中。
    httpclient.getParams.setParameter("http.socket.timeout", 2000)
    httpclient.getParams.setParameter("http.connection.timeout", 2000)
    httpclient.getParams.setParameter("http.connection-manager.timeout", 2000L)

    try {
      val status_code = httpclient.executeMethod(httpget)

      if(status_code != HttpStatus.SC_OK) 
        Nil
      else
        httpget.getResponseBody.toList
    } catch {
      case e : Exception => {
        // 对于httpclient的任何异常，都在这里接住，返回空表，表示什么也
        // 没有下载到。
        // Log.error("download", e)
        return Nil
      }
    } finally {
      httpget.releaseConnection
    }
  }
}

class MmsRequest {
  val title : String = null
  val to : String = null
  val parts : Array[MmsRequestPart] = null
}
