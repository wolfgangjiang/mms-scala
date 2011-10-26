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
