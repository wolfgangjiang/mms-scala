package com.edoctor.mms

import SessionHelpers._
import SessionParameters._

trait AbstractRemote {
  def with_telnet_connection(modem_ip : String, modem_port : Int) 
    (block : AbstractDuplex => Unit) : Unit
}

trait ActualRemote extends AbstractRemote {
  def with_telnet_connection(modem_ip : String, modem_port : Int) 
    (block : AbstractDuplex => Unit) : Unit = {
    val tc = new org.apache.commons.net.telnet.TelnetClient
    try {
      tc.connect(modem_ip, modem_port)
      block(new ActualDuplex(tc.getInputStream, tc.getOutputStream))
    } finally {
      try {
        tc.disconnect
      } catch {
        case e : Exception =>
          e.printStackTrace
      }
    }
  }
}

abstract class AbstractDuplex {
  def say_text(command : String) : Unit
  def listen_text(timeout_millis : Long) : String
  def read_binary(timeout_millis : Long) : List[Byte]
}

class ActualDuplex(val in_s : java.io.InputStream,
                   val out_s : java.io.OutputStream) 
extends AbstractDuplex {
  def say_text(command : String) : Unit = {
    out_s.write((command + "\r").getBytes)
    out_s.flush
  }

  // 阻塞读取，直到超时，把所读到的内容返回。
  // 但如果读到0x7e（'~'），却立刻返回，不吃掉紧随其后的二进制数据包。
  def listen_text(timeout_millis : Long) : String = {
    val start_time = System.currentTimeMillis

    def listen_with(data : String) : String = {
      Thread.sleep(burst_read_interval)
      if(System.currentTimeMillis - start_time > timeout_millis)
        data                            // read immediately
      else if(in_s.available == 0)
        listen_with(data)               // continue wait to read
      else {
        val ch = in_s.read.toChar
        if(ch == '~')
          data + ch  // return immediately, do not consume binary data
        else
          listen_with(data + ch)
      }      
    }

    listen_with("").trim
  }

  def read_binary(timeout_millis : Long) : List[Byte] = {
    val start_time = System.currentTimeMillis

    def read_with(data : List[Byte]) : List[Byte] = {
      if(System.currentTimeMillis - start_time > timeout_millis)
        data                            // return immediately
      else if(in_s.available == 0)
        read_with(data)                 // continue wait to read
      else {
        val octet = in_s.read.toByte
        if(octet != 0x7e.toByte) 
          read_with(octet :: data)
        else if(data.isEmpty)
          read_with(data) // silently skip empty frame and continue read
        else
          data
      }        
    }

    decode_0x7d_escape(read_with(Nil).reverse)
  }
}

case class PppFrame(protocol : Protocol,
                    payload : FramePayload)

trait FramePayload

sealed abstract class Protocol
object Protocol {
  case object LCP extends Protocol
  case object PAP extends Protocol
  case object IPCP extends Protocol
  case object IP extends Protocol
  case object Unknown extends Protocol
}

// wrap a string in FramePayload trait
class ErrorMessage(val get : String) extends FramePayload {
  override def toString = get
}

class LcpPacket(raw_payload : List[Byte]) extends FramePayload {
  import LcpCode._
  val code_names = Map[Byte, LcpCode](1.toByte -> ConfigureRequest,
                                      2.toByte -> ConfigureAck,
                                      3.toByte -> ConfigureNak,
                                      5.toByte -> TerminateRequest,
                                      6.toByte -> TerminateAck)

  val code : LcpCode = get_code_name(raw_payload(0))
  val identifier : Byte = raw_payload(1)
  val length : Int = byte_list_to_int(raw_payload.slice(2,4))
  val data : List[Byte] = raw_payload.take(length).drop(4)

  def get_code_name(code_byte : Byte) : LcpCode = {
    if(code_names contains code_byte)
      code_names(code_byte)
    else
      Unknown
  }
  
  override def toString : String = {
    "LCP(" + code + 
    "; id: " + to_hex_string(List(identifier)) + 
    "; data: " + to_hex_string(data) + ")"
  }
}

sealed abstract class LcpCode
object LcpCode {
  case object ConfigureRequest extends LcpCode
  case object ConfigureAck extends LcpCode
  case object ConfigureNak extends LcpCode
  case object TerminateRequest extends LcpCode
  case object TerminateAck extends LcpCode
  case object Unknown extends LcpCode
}

object Ppp {
    // fcs相关的函数，都是从rfc 1662附录所规定的循环冗余校验代码中改写过
  // 来的。在rfc 1662标准的附录中，有C语言写的求校验的代码。
  private val fcs_table_16 = calculate_fcs_table_16
  
  // 本函数仅仅在模块初始化时运行一次，函数的结果作为缓存，以加速以后的
  // 校验运算。
  private def calculate_fcs_table_16():List[Int] = {
    var v = 0
    var the_table = List[Int]()

    for(b <- 0 until 256) {
      v = b
      for(i <- 1 to 8) 
        v = 
          if((v & 1) != 0) 
            ((v >> 1) ^ 0x8408) 
          else 
            v >> 1
      the_table = (v & 0xFFFF) :: the_table
    }
    the_table.reverse
  }     

  def get_fcs16(data : List[Byte]) : Int = {
    var fcs = 0xFFFF
    
    for(octet <- data) 
      fcs = ((fcs >> 8) ^ fcs_table_16((fcs ^ octet) & 0xff)) & 0xffff

    (~fcs) & 0xffff
  }

  def is_fcs16_good(data : List[Byte]) : Boolean = {
    var fcs = 0xFFFF
    
    for(octet <- data) 
      fcs = ((fcs >> 8) ^ fcs_table_16((fcs ^ octet) & 0xff)) & 0xffff
    
    fcs == 0xf0b8  // magic number
  }

  // 校验和的尾端特性与普通数据相反，所以split_word后要reverse一下。
  def attach_fcs16(data : List[Byte]) : List[Byte] = 
    data ++ split_word(get_fcs16(data)).reverse  

  private val protocol_indicators : Map[List[Byte], Protocol] =
    Map(parse_hex("C0 21") -> Protocol.LCP,
        parse_hex("C0 23") -> Protocol.PAP,
        parse_hex("80 21") -> Protocol.IPCP,
        parse_hex("00 21") -> Protocol.IP)

  // 四种情况返回失败：第一，数据包长度不足6字节；第二，开头两字节不是
  // FF 03；第三，校验失败；第四，不认识的协议字段。
  def parse_frame(raw_frame : List[Byte]) : PppFrame = {
    def error(msg : String) : PppFrame = 
      new PppFrame(Protocol.Unknown, new ErrorMessage(msg))    

    if(raw_frame.length < 6) 
      error("frame length less than 6")
    else if(raw_frame.take(2) != List(0xFF.toByte, 0x03.toByte)) 
      error("initial two bytes are not FF 03")
    else if(!is_fcs16_good(raw_frame)) 
      error("fcs 16 checksum failed")
    else if(!protocol_indicators.contains(raw_frame.slice(2,4))) 
      error("unknown protocol field " + 
            to_hex_string(raw_frame.slice(2,4)))
    else {
      val protocol = protocol_indicators(raw_frame.slice(2,4))
      val payload_data = raw_frame.drop(4).dropRight(2)
      val payload_packet = protocol match {
        case Protocol.LCP => new LcpPacket(payload_data)
        case _ => new ErrorMessage("not implemented yet")
      }
      new PppFrame(protocol, payload_packet)
    }
  }
}
