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
  def read_ppp(timeout_millis : Long) : PppFrame
  def write_ppp(frame : PppFrame) : Unit
}

// incoming terminate request should be handled by low level duplex
// and throw an "ClosedByRemoteException" to indicate total failure.
// this class is not covered by mocked unit tests
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
        data                            // return immediately
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

  // 阻塞读取，直到读到一个完整的、非空的ppp frame（在两个分界符0x7e之
  // 间），如果超时，则无论已经读到多少，都返回Protocol.Timeout，这是因
  // 为无论已经读到多少，都不是完整的frame。如果是连续的两个0x7e之间，
  // 这样的空frame会丢弃，不会报错，也不会返回空List。这个函数还检查来
  // 自于远端的lcp terminate request，一旦检查到，就同意关闭连接，并且
  // 抛出“连接被远端关闭”的异常。
  def read_ppp(timeout_millis : Long) : PppFrame = {
    val start_time = System.currentTimeMillis

    def read_with(data : List[Byte]) : PppFrame = {
      if(System.currentTimeMillis - start_time > timeout_millis)
        new PppFrame(Protocol.Timeout,   // fail immediately
                     new ErrorMessage("time out in read_ppp()"))
      else if(in_s.available == 0)
        read_with(data)                 // continue wait to read
      else {
        val octet = in_s.read.toByte
        if(octet != 0x7e.toByte) 
          read_with(octet :: data)
        else if(data.isEmpty)
          read_with(data) // silently skip empty frame and continue read
        else {
          println("received: " + 
                  to_hex_string(decode_0x7d_escape(data.reverse)))
          PppFrame.parse(data.reverse)
        }
      }        
    }

    val frame = read_with(Nil)
    check_terminate_request(frame)
    frame
  }

  def write_ppp(frame : PppFrame) : Unit = {
    println("sent: " + 
            to_hex_string(decode_0x7d_escape(frame.bytes.init.tail)))
    out_s.write(frame.bytes.toArray)
    out_s.flush
  }

  // this works like a before filter in Ruby on Rails
  // further process is prevented if some condition is met
  // (in this case, if receiving a terminate request)
  private def check_terminate_request(frame : PppFrame) : Unit = {
    if(frame.protocol == Protocol.LCP) {
      val incoming_packet = frame.payload.asInstanceOf[LcpPacket]
      if(incoming_packet.code == LcpCode.TerminateRequest) {
        val ack_packet = new LcpPacket(
          LcpCode.TerminateAck, incoming_packet.identifier, Nil)
        val ack_frame = new PppFrame(Protocol.LCP, ack_packet)
        write_ppp(ack_frame)
        throw new ClosedByRemoteException
      }
    }
  }
}

class SessionTimeoutException extends RuntimeException
class InvalidComposeException extends RuntimeException
class ClosedByRemoteException extends RuntimeException
class AuthenticateFailureException extends RuntimeException

case class PppFrame(protocol : Protocol,
                    payload : FramePayload) {
  def bytes : List[Byte] = {
    if(protocol == Protocol.Unknown || payload.isInstanceOf[ErrorMessage])
      throw new InvalidComposeException
    else {
      val protocol_field_bytes = 
        PppFrame.protocol_indicators.map(_.swap).apply(protocol)
      val frame_bytes = PppFrame.attach_fcs16(
        parse_hex("FF 03") ++ protocol_field_bytes ++ payload.bytes)
      0x7e.toByte +: encode_0x7d_escape(frame_bytes) :+ 0x7e.toByte
    }      
  }
}

trait FramePayload {
  def bytes : List[Byte]
}

sealed abstract class Protocol
object Protocol {
  case object LCP extends Protocol
  case object PAP extends Protocol
  case object IPCP extends Protocol
  case object IP extends Protocol
  case object Timeout extends Protocol
  case object Unknown extends Protocol
}

// wrap a string in FramePayload trait
class ErrorMessage(val get : String) extends FramePayload {
  override def toString = get
  override def bytes = throw new InvalidComposeException
}

object PppFrame {
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

  val protocol_indicators : Map[List[Byte], Protocol] =
    Map(parse_hex("C0 21") -> Protocol.LCP,
        parse_hex("C0 23") -> Protocol.PAP,
        parse_hex("80 21") -> Protocol.IPCP,
        parse_hex("00 21") -> Protocol.IP)

  // 四种情况返回失败：第一，数据包长度不足6字节；第二，开头两字节不是
  // FF 03；第三，校验失败；第四，不认识的协议字段。
  def parse(raw_bytes : List[Byte]) : PppFrame = {
    val raw_frame = decode_0x7d_escape(raw_bytes)
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
        case Protocol.LCP => LcpPacket.parse(payload_data)
        case Protocol.PAP => PapPacket.parse(payload_data)
        case _ => new ErrorMessage("not implemented yet")
      }
      new PppFrame(protocol, payload_packet)
    }
  }
}

class PppIdCounter(initial : Int) {
  private var counter : Int = initial 
  def get_id : Byte = { // 它只有1个字节。
    counter += 1
    if(counter > 0x30)
      counter = 0x10
    counter.toByte
  }
}

sealed abstract class LcpCode
object LcpCode {
  case object ConfigureRequest extends LcpCode
  case object ConfigureAck extends LcpCode
  case object TerminateRequest extends LcpCode
  case object TerminateAck extends LcpCode
  case object Unknown extends LcpCode
}

sealed abstract class LcpState
object LcpState {
  object Closed extends LcpState
  object ReqSent extends LcpState
  object AckSent extends LcpState
  object AckRcvd extends LcpState  
  object Ready extends LcpState
  object Closing extends LcpState
}

object LcpPacket {
  import LcpCode._
  val code_names = Map[Byte, LcpCode](1.toByte -> ConfigureRequest,
                                      2.toByte -> ConfigureAck,
                                      5.toByte -> TerminateRequest,
                                      6.toByte -> TerminateAck)

  private def get_code_name(code_byte : Byte) : LcpCode = {
    code_names.getOrElse(code_byte, Unknown)
  }
  
  def parse(raw_bytes : List[Byte]) : LcpPacket = {
    val code : LcpCode = get_code_name(raw_bytes(0))
    val identifier : Byte = raw_bytes(1)
    val length : Int = byte_list_to_int(raw_bytes.slice(2,4))
    val data : List[Byte] = raw_bytes.take(length).drop(4)

    new LcpPacket(code, identifier, data)
  }
}

class LcpPacket(val code : LcpCode, 
                val identifier : Byte, 
                val data : List[Byte]) extends FramePayload {
  val length : Int = data.length + 4
  
  override def toString : String = {
    "LCP(" + code + 
    "; id: 0x" + to_hex_string(List(identifier)) + 
    "; data: " + to_hex_string(data) + ")"
  }

  def bytes : List[Byte] = {
    if(code == LcpCode.Unknown)
      throw new InvalidComposeException
    else {
      val code_byte = LcpPacket.code_names.map(_.swap).apply(code)
      val length_bytes = split_word(length)
      code_byte :: identifier :: length_bytes ++ data
    }
  }
}

class LcpAutomaton(val duplex : AbstractDuplex, 
                   val id_counter : PppIdCounter) {
  import LcpState._

  sealed abstract class LcpEvent
  object LcpEvent {
    object OpenConnection extends LcpEvent
    object CloseConnection extends LcpEvent
    object ReceiveConfigReq extends LcpEvent
    object ReceiveConfigAck extends LcpEvent
    object ReceiveTerminateReq extends LcpEvent
    object ReceiveTerminateAck extends LcpEvent
    object SoftTimeout extends LcpEvent
    object HardTimeout extends LcpEvent
  }
  import LcpEvent._

  abstract class LcpAction {
    def perform : Unit
  }

  private var the_state : LcpState = Closed
  private var retransmit_counter : Int = max_retransmit_times
  private var the_recent_received_packet : LcpPacket = null
  private var the_recent_sent_packet : LcpPacket = null

  val no_op_action = new LcpAction { def perform : Unit = {} }
  val get_closed_by_remote_action = new LcpAction { 
    def perform : Unit = {
      send_terminate_ack
      the_state = Closed
      throw new ClosedByRemoteException
    }
  }
  val retransmit_action = new LcpAction {
    def perform : Unit = {
      if(recent_sent_packet != null) {
        val retransmit_packet = new LcpPacket(
          recent_sent_packet.code,
          id_counter.get_id,  // a new id
          recent_sent_packet.data)
        send_packet(retransmit_packet)
      }
      retransmit_counter -= 1
      read_from_remote
    }
  }
  val hard_timeout_action = new LcpAction {
    def perform : Unit = {
      the_state = Closed
      throw new SessionTimeoutException
    }
  }

  val packet_driven_events = Map[LcpCode, LcpEvent](
    LcpCode.ConfigureRequest -> ReceiveConfigReq,
    LcpCode.ConfigureAck -> ReceiveConfigAck,
    LcpCode.TerminateRequest -> ReceiveTerminateReq,
    LcpCode.TerminateAck -> ReceiveTerminateAck)
  
  val state_transition_table = Map(  
    (Closed, OpenConnection) -> new LcpAction { def perform : Unit = {
        initialize_retransmit_counter
        send_config_request
        the_state = ReqSent
        read_from_remote
    }},
    (ReqSent, ReceiveConfigReq) -> new LcpAction { def perform : Unit = {
        send_config_ack
        the_state = AckSent
        read_from_remote
    }},
    (ReqSent, ReceiveConfigAck) -> new LcpAction { def perform : Unit = {
        initialize_retransmit_counter
        the_state = AckRcvd
        read_from_remote
    }},
    (AckSent, ReceiveConfigAck) -> new LcpAction { def perform : Unit = {
        initialize_retransmit_counter
        the_state = Ready
    }},
    (AckRcvd, ReceiveConfigReq) -> new LcpAction { def perform : Unit = {
        initialize_retransmit_counter
        send_config_ack
        the_state = Ready
    }},
    (Ready, CloseConnection) -> new LcpAction { def perform : Unit = {
        initialize_retransmit_counter
        send_terminate_req
        the_state = Closing
        read_from_remote
    }},
    (Closing, ReceiveTerminateAck) -> new LcpAction { def perform : Unit = {
        the_state = Closed
    }},
    (ReqSent, SoftTimeout) -> retransmit_action,
    (AckSent, SoftTimeout) -> retransmit_action,
    (AckRcvd, SoftTimeout) -> retransmit_action,
    (Closing, SoftTimeout) -> retransmit_action
  )

  def state = the_state
  def recent_received_packet = the_recent_received_packet
  def recent_sent_packet = the_recent_sent_packet

  def set_state(s : LcpState) : Unit = { // for testing
    the_state = s 
  } 


  def feed_event(event : LcpEvent) : Unit = {
    val action = if(event == ReceiveTerminateReq) {
      get_closed_by_remote_action
    } else if(event == HardTimeout) {
      hard_timeout_action
    } else if(!(state_transition_table contains (state, event))) {
      no_op_action
    } else {
      state_transition_table(state, event)
    }

    action.perform
  }

  def open : Unit = {
    this.feed_event(OpenConnection)
  }

  def close : Unit = {
    this.feed_event(CloseConnection)
  }

  def read_from_remote : Unit = {
    val frame = duplex.read_ppp(request_timeout_interval)
    if(frame.protocol == Protocol.Timeout) {
      if(retransmit_counter > 0) {
        this.feed_event(SoftTimeout)
      } else this.feed_event(HardTimeout)
    } else if(frame.protocol != Protocol.LCP) {
      read_from_remote                // silently discard this frame
    } else {
      val packet = frame.payload.asInstanceOf[LcpPacket]
      if(packet.code == LcpCode.Unknown)
        read_from_remote              // silently discard this frame
      else {
        the_recent_received_packet = packet
        this.feed_event(packet_driven_events(packet.code))
      }
    }
  }

  def initialize_retransmit_counter : Unit = {
    retransmit_counter = max_retransmit_times
  }

  def send_config_request : Unit = {
    val packet = new LcpPacket(
      LcpCode.ConfigureRequest, 
      id_counter.get_id, 
      our_lcp_config_req_options)

    send_packet(packet)
  }

  def send_config_ack : Unit = {
    val packet = new LcpPacket(
      LcpCode.ConfigureAck, 
      recent_received_packet.identifier,
      recent_received_packet.data)
    
    send_packet(packet)
  }

  def send_terminate_req : Unit = {
    val packet = new LcpPacket(
      LcpCode.TerminateRequest, id_counter.get_id, Nil)

    send_packet(packet)
  }

  def send_terminate_ack : Unit = {
    val packet = new LcpPacket(
      LcpCode.TerminateAck, recent_received_packet.identifier, Nil)

    send_packet(packet)
  }

  def send_packet(packet : LcpPacket) : Unit = {
    the_recent_sent_packet = packet
    duplex.write_ppp(new PppFrame(Protocol.LCP, packet))
  }
}


sealed abstract class PapCode
object PapCode {
  case object AuthenticateRequest extends PapCode
  case object AuthenticateAck extends PapCode
  case object AuthenticateNak extends PapCode
  case object Unknown extends PapCode
}

object PapPacket {
  import PapCode._
  val code_names = Map[Byte, PapCode](1.toByte -> AuthenticateRequest,
                                      2.toByte -> AuthenticateAck,
                                      3.toByte -> AuthenticateNak)

  private def get_code_name(code_byte : Byte) : PapCode = {
    code_names.getOrElse(code_byte, Unknown)
  }

  def parse(raw_bytes : List[Byte]) : PapPacket = {
    val code : PapCode = get_code_name(raw_bytes(0))
    val identifier : Byte = raw_bytes(1)
    val length : Int = byte_list_to_int(raw_bytes.slice(2,4))
    val data : List[Byte] = raw_bytes.take(length).drop(4)
    
    new PapPacket(code, identifier, data)
  }
}

class PapPacket(val code : PapCode,
                val identifier : Byte,
                val data : List[Byte]) extends FramePayload {
  val length : Int = data.length + 4

  override def toString : String = {
    "PAP(" + code +
    "; id: 0x" + to_hex_string(List(identifier)) +
    "; data: " + to_hex_string(data) + ")"
  }

  def bytes : List[Byte] = {
    if(code == PapCode.Unknown)
      throw new InvalidComposeException
    else {
      val code_byte = PapPacket.code_names.map(_.swap).apply(code)
      val length_bytes = split_word(length)
      code_byte :: identifier :: length_bytes ++ data
    }
  }
}

class PapAutomaton(val duplex : AbstractDuplex,
                   val id_counter : PppIdCounter) {
  private var the_recent_sent_packet : PapPacket = null
  def recent_sent_packet = the_recent_sent_packet
  private var the_request_sent_time : Long = System.currentTimeMillis
  def request_sent_time = the_request_sent_time
  private var retransmit_counter = max_retransmit_times
  
  def timeout = {
    (System.currentTimeMillis - request_sent_time > 
     request_timeout_interval)
  }
    
  def authenticate : Unit = {
    val auth_req = new PapPacket(PapCode.AuthenticateRequest, 
                                 id_counter.get_id, 
                                 our_pap_auth_req_info)
    send_packet(auth_req)
    read_from_remote_and_process
  }

  def read_from_remote_and_process : Unit = {
    val frame = duplex.read_ppp(request_timeout_interval)

    // if there is a constant flow of irrelevent packets, 
    // then we will not get a Protocol.Timeout, in such circumstances,
    // we check timeout with our private timer.
    if((frame.protocol != Protocol.PAP && timeout) ||
       frame.protocol == Protocol.Timeout) {
      if(retransmit_counter > 0)
        retransmit   // soft timeout
      else
        throw new SessionTimeoutException  // hard timeout
    }
    else if(frame.protocol != Protocol.PAP) // and not timeout yet
      read_from_remote_and_process // silently discard
    else {
      val packet = frame.payload.asInstanceOf[PapPacket]
      if(packet.code == PapCode.AuthenticateAck) {
        // success, return without incident
      }
      else if(packet.code == PapCode.AuthenticateNak)
        throw new AuthenticateFailureException
      else
        read_from_remote_and_process // silently discard
    }
  }

  def retransmit : Unit = {
    if(recent_sent_packet != null) {
      val retransmit_packet = new PapPacket(
        recent_sent_packet.code,
        id_counter.get_id,  // a new id
        recent_sent_packet.data)
      send_packet(retransmit_packet)
    }
    retransmit_counter -= 1
    read_from_remote_and_process
  }

  def send_packet(packet : PapPacket) : Unit = {
    the_recent_sent_packet = packet
    the_request_sent_time = System.currentTimeMillis
    duplex.write_ppp(new PppFrame(Protocol.PAP, packet))
  }
}
