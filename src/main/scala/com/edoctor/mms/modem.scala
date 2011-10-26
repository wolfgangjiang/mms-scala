package com.edoctor.mms

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
  def listen_text(timeout_millis : Long) : String = {
    val start_time = System.currentTimeMillis
    val data = new StringBuffer

    while(System.currentTimeMillis - start_time < timeout_millis) {
      if(in_s.available > 0) 
        data.append(in_s.read.toChar)
    }

    data.toString.trim      
  }

  def read_binary(timeout_millis : Long) : List[Byte] = {
    val start_time = System.currentTimeMillis
    var data = List[Byte]()
    var going = true

    while(System.currentTimeMillis - start_time < timeout_millis && going) {
      if(in_s.available > 0) {
        val octet = in_s.read.toByte
        if(octet != 0x7e.toByte) {
          data = octet :: data
        } else if(data.length > 0) {
            going = false
        } // else do not append and wait for next
      }
    }

    data.reverse
  }
}
