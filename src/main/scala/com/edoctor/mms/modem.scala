package com.edoctor.mms

import SessionHelpers._
import SessionParameters._

// =============================
// telnet connection and duplex
// =============================

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
      block(new ActualDuplex(tc.getInputStream, tc.getOutputStream,
                             modem_ip + ":" + modem_port.toString))
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

abstract class AbstractDuplex(initial_id : Int) {
  def say_text(command : String) : Unit
  def listen_text(timeout_millis : Long) : String
  def read_ppp(timeout_millis : Long) : PppFrame
  def write_ppp(frame : PppFrame) : Unit
  def name : String

  private var id_counter : Int = initial_id
  def get_ppp_id : Byte = { // 它只有1个字节。
    id_counter += 1
    // set a cap to avert complications on negative Byte numbers
    if(id_counter > 0x30)  
      id_counter = 0x10
    id_counter.toByte
  }
}

// incoming terminate request should be handled by low level duplex
// and throw an "ClosedByRemoteException" to indicate total failure.
// this class is not covered by mocked unit tests
class ActualDuplex(val in_s : java.io.InputStream,
                   val out_s : java.io.OutputStream,
                   override val name : String)
extends AbstractDuplex(initial_ppp_id) {
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
          Log.bytes(name,
            "received: " + to_hex_string(decode_0x7d_escape(data.reverse)))
          PppFrame.parse(data.reverse)
        }
      }        
    }

    val frame = read_with(Nil)
    check_terminate_request(frame)
    frame
  }

  def write_ppp(frame : PppFrame) : Unit = {
    Log.bytes(name, 
      "sent: " + to_hex_string(decode_0x7d_escape(frame.bytes.init.tail)))
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
class MalformedIpAddressException extends RuntimeException
class DialFailedException extends RuntimeException

// =============================
// PPP protocol
// =============================

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
        case Protocol.IPCP => IpcpPacket.parse(payload_data)
        case Protocol.IP => IpDatagram.parse(payload_data)
        case _ => throw new InvalidComposeException
      }
      new PppFrame(protocol, payload_packet)
    }
  }
}

// =============================
// LCP protocol
// =============================

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

// LCP建立连接，协议的规定是，双方对等，都主动向对方发送config
// request，在接收到config request时，发送一个id相同的config ack，当
// 双方都收到config ack，就是连接建立成功。如果没有收到config ack，就
// 重传config request，直到超出重传次数限制。如果对方的options中有些
// 我们不能接受，我们还可以发送config nak去回绝它，但是发送彩信时我们
// 遇到的对方options都可以接受。
// LCP结束连接。按照协议规定，任何一方可以单方面发起结束连接，发送
// terminate request，对方应回复terminate ack。如果成功收到terminate
// ack，连接成功结束，否则重传。
class LcpAutomaton(val duplex : AbstractDuplex) {
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

  abstract class LcpAction { def perform : Unit }

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
          duplex.get_ppp_id,  // a new id
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
      Log.warn_unhandled_packet(duplex.name, frame.bytes)
      read_from_remote                // silently discard this frame
    } else {
      val packet = frame.payload.asInstanceOf[LcpPacket]
      if(packet.code == LcpCode.Unknown) {
        Log.warn_unhandled_packet(duplex.name, frame.bytes)
        read_from_remote              // silently discard this frame
      } else {
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
      duplex.get_ppp_id, 
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
      LcpCode.TerminateRequest, duplex.get_ppp_id, Nil)

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


// =============================
// PAP protocol
// =============================


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

// 彩信网关（代理？）的连接不需要用户名和密码，pap的用户名和密码都设置
// 为单字节"00"。只要得到了auth ack就成功。
class PapAutomaton(val duplex : AbstractDuplex) {
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
                                 duplex.get_ppp_id, 
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
    else if(frame.protocol != Protocol.PAP) {// and not timeout yet
      Log.warn_unhandled_packet(duplex.name, frame.bytes)
      read_from_remote_and_process // silently discard
    } else {
      val packet = frame.payload.asInstanceOf[PapPacket]
      if(packet.code == PapCode.AuthenticateAck) {
        // success, do nothing and return without incident
      }
      else if(packet.code == PapCode.AuthenticateNak)
        throw new AuthenticateFailureException
      else {
        Log.warn_unhandled_packet(duplex.name, frame.bytes)
        read_from_remote_and_process // silently discard
      }
    }
  }

  def retransmit : Unit = {
    if(recent_sent_packet != null) {
      val retransmit_packet = new PapPacket(
        recent_sent_packet.code,
        duplex.get_ppp_id,  // a new id
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

// =============================
// IPCP protocol
// =============================


sealed abstract class IpcpCode
object IpcpCode {
  case object ConfigureRequest extends IpcpCode
  case object ConfigureAck extends IpcpCode
  case object ConfigureNak extends IpcpCode
  case object Unknown extends IpcpCode
}

object IpcpPacket {
  import IpcpCode._
  val code_names = Map[Byte, IpcpCode](1.toByte -> ConfigureRequest,
                                       2.toByte -> ConfigureAck,
                                       3.toByte -> ConfigureNak)

  private def get_code_name(code_byte : Byte) : IpcpCode = {
    code_names.getOrElse(code_byte, Unknown)
  }

  def parse(raw_bytes : List[Byte]) : IpcpPacket = {
    val identifier : Byte = raw_bytes(1)
    val length : Int = byte_list_to_int(raw_bytes.slice(2,4))
    val data : List[Byte] = raw_bytes.take(length).drop(4)
    val ip_list : List[Byte] = 
      if(data.length == 6 && data.slice(0,2) == parse_hex("03 06"))
        data.drop(2)
      else
        null
    val code : IpcpCode = // both unknown code and malformed ip are Unknown 
      if(ip_list == null)
        Unknown
      else
        get_code_name(raw_bytes(0))

    new IpcpPacket(code, identifier, ip_list)
  }
}

class IpcpPacket(val code : IpcpCode,
                 val identifier : Byte,
                 val ip_list : List[Byte]) extends FramePayload {
  val length : Int = 10 // 4 + "03 06" + ip_list, where ip_list.length == 4

  override def toString : String = {
    "IPCP(" + code +
    "; id: 0x" + to_hex_string(List(identifier)) +
    "; ip_addr: " + ip_to_string(ip_list)
  }

  def bytes : List[Byte] = {
    if(code == IpcpCode.Unknown)
      throw new InvalidComposeException
    else {
      val code_byte = IpcpPacket.code_names.map(_.swap).apply(code)
      val length_bytes = split_word(length)
      (code_byte :: identifier :: length_bytes ++ 
       parse_hex("03 06") ++ ip_list)
    }
  }
}

// IPCP协议交互的任务是获得一个对方分配的IP地址。过程是这样的：(1)我
// 们发送一个config req，请求ip地址0.0.0.0，同时对方主动发给我们一个
// config req，声明它自己的ip地址，在我们在翼多的调试中总是
// 192.168.111.111。这个对方的ip地址是无线路由器的ip地址，因为现在我
// 们还在ppp阶段，只是一个local network，没有ip地址，是不能够穿过路由
// 器到达外面广大的internetwork中的。(2)我们认可对方的ip，发送一个
// 192.168.111.111的config ack给它。同时，对方驳回我们的请求，发给我
// 们一个config nak，毕竟0.0.0.0是不可能分配给我们的。在config
// nak中，会包含一个对方建议我们采用（分配给我们）的ip，是随机的，在
// 我们国内一般都是10.*.*.*。(3)我们用它给我们的ip地址来再次申请，发
// 送config req，它让我们申请哪个，我们就申请哪个。这样，它就会返回
// config ack，认可我们的申请。然后，我们就可以开始使用这个ip地址了。
class IpcpAutomaton(val duplex : AbstractDuplex) {
  private var recent_sent_packet : IpcpPacket = null
  private var recent_received_packet : IpcpPacket = null
  private var my_ip_list : List[Byte] = null

  private var recent_sent_time : Long = System.currentTimeMillis
  private var retransmit_counter = max_retransmit_times
  
  def timeout = {
    (System.currentTimeMillis - recent_sent_time > 
     request_timeout_interval)
  }

  def get_ip : List[Byte] = {
    initialize_retransmit_counter
    send_first_config_request
    read_from_remote_and_process
  }

  def initialize_retransmit_counter : Unit = {
    retransmit_counter = max_retransmit_times
  }

  def read_from_remote_and_process : List[Byte] = {
    val frame = duplex.read_ppp(request_timeout_interval)

    if((frame.protocol != Protocol.IPCP && timeout) ||
       frame.protocol == Protocol.Timeout) {
      if(retransmit_counter > 0) {
        retransmit   // soft timeout
        read_from_remote_and_process
      }
      else
        throw new SessionTimeoutException  // hard timeout      
    } 
    else if(frame.protocol != Protocol.IPCP) { // but not timeout
      Log.warn_unhandled_packet(duplex.name, frame.bytes)
      read_from_remote_and_process      // silently discard
    } else {
      val packet = frame.payload.asInstanceOf[IpcpPacket]
      recent_received_packet = packet
      if(packet.code == IpcpCode.ConfigureRequest) {
        send_configure_ack
        read_from_remote_and_process
      } 
      else if(packet.code == IpcpCode.ConfigureNak) {
        initialize_retransmit_counter
        my_ip_list = packet.ip_list
        send_second_config_request
        read_from_remote_and_process
      }
      else if(packet.code == IpcpCode.ConfigureAck) {
        my_ip_list // return, this is the only exit of this function
      }
      else
        read_from_remote_and_process
    }
  }

  def send_first_config_request : Unit = {
    val first_config_req = new IpcpPacket(
      IpcpCode.ConfigureRequest, 
      duplex.get_ppp_id, 
      parse_hex("00 00 00 00"))
    
    send_packet(first_config_req)
  }

  def send_second_config_request : Unit = {
    val second_config_req = new IpcpPacket(
      IpcpCode.ConfigureRequest, 
      duplex.get_ppp_id, 
      my_ip_list)
    
    send_packet(second_config_req)
  }

  def send_configure_ack : Unit = {
    val config_ack = new IpcpPacket(
      IpcpCode.ConfigureAck,
      recent_received_packet.identifier,
      recent_received_packet.ip_list)

    send_packet(config_ack)
  }

  def retransmit : Unit = {
    if(recent_sent_packet != null) {
      val retransmit_packet = new IpcpPacket(
        recent_sent_packet.code,
        duplex.get_ppp_id,  // a new id
        recent_sent_packet.ip_list)
      send_packet(retransmit_packet)
    }
    retransmit_counter -= 1
  }

  def send_packet(packet : IpcpPacket) : Unit = {
    recent_sent_time = System.currentTimeMillis
    recent_sent_packet = packet
    duplex.write_ppp(new PppFrame(Protocol.IPCP, packet))    
  }
}

// =============================
// IP protocol
// =============================
class IpData(val bytes : List[Byte]) extends FramePayload

class IpDatagram(val identification : Int,
                 val source_ip_addr : List[Byte],
                 val destination_ip_addr : List[Byte],
                 val segment : UdpDatagram) {
  val segment_bytes = segment.bytes(source_ip_addr, destination_ip_addr)
  val total_length = segment_bytes.length + 20 // header length == 20

  // bytes without checksum ( checksum == 0 )
  private def get_holo_header_bytes : List[Byte] = {
    parse_hex("45 00") ++ // version, header len, "type of service"
    split_word(total_length) ++  // total length
    split_word(identification) ++ 
    split_word(0) ++ // we won't make fragments
    List(default_ip_ttl, 0x11.toByte) ++ // ttl, protocol = udp = 0x11
    split_word(0) ++ // dummy checksum to be filled in later
    source_ip_addr ++ 
    destination_ip_addr
  }

  def bytes : List[Byte] = {
    val holo_header_bytes = get_holo_header_bytes
    val checksum = compute_checksum(holo_header_bytes)
    val header_bytes = holo_header_bytes.patch(10, split_word(checksum), 2)
    header_bytes ++ segment_bytes
  }
}

object IpDatagram {
  def is_header_checksum_good(bytes : List[Byte]) : Boolean = {
    (bytes.length >= 20) && compute_checksum(bytes.take(20)) == 0
  }

  // to be further unpacked in UdpDuplex
  def parse(bytes : List[Byte]) : IpData = new IpData(bytes)

  def unpack(bytes : List[Byte]) : Either[String, List[Byte]] = {
    if(is_header_checksum_good(bytes)) {
      if(bytes(9) == 0x11)
        Right(bytes.drop(20))
      else
        Left("Not UDP protocol")
    }
    else
      Left("Corrupted")
  }
}

// =============================
// UDP protocol
// =============================
class UdpDatagram(val source_port : Int,
                  val destination_port : Int,
                  val data : List[Byte]) {
  val length = data.length + 8

  // this is with checksum field set to zero
  private def holo_bytes : List[Byte] = {
    split_word(source_port) ++
    split_word(destination_port) ++
    split_word(length) ++
    split_word(0) ++ // checksum field
    data
  }

  // this is with pseudo header 
  private def compute_udp_checksum(
    source_ip_addr : List[Byte],
    destination_ip_addr : List[Byte]) : Int = {
    val pseudo_header = UdpDatagram.make_pseudo_header(
      source_ip_addr, destination_ip_addr, length)
      
    compute_checksum(pseudo_header ++ holo_bytes)
  }


  // need ip addresses to compute checksum, because checksum takes a
  // pseudo header into account.
  def bytes(source_ip_addr : List[Byte],
            destination_ip_addr : List[Byte]) : List[Byte] = {
    val checksum = compute_udp_checksum(source_ip_addr,
                                        destination_ip_addr)
    holo_bytes.patch(6, split_word(checksum), 2)
  }
}

object UdpDatagram {
  def unpack(bytes : List[Byte],
             source_ip_addr : List[Byte],
             destination_ip_addr : List[Byte]) : Either[String, List[Byte]] = {
    if(is_checksum_good(bytes, source_ip_addr, destination_ip_addr))
      Right(bytes.drop(8))
    else
      Left("Corrupted")
  }

  def is_checksum_good(bytes : List[Byte],
                       source_ip_addr : List[Byte],
                       destination_ip_addr : List[Byte]) : Boolean = {
    val pseudo_header = 
      make_pseudo_header(source_ip_addr, destination_ip_addr, bytes.length)

    (bytes.length >= 8) && (compute_checksum(pseudo_header ++ bytes) == 0)
  }

  def make_pseudo_header(source_ip_addr : List[Byte],
                         destination_ip_addr : List[Byte],
                         length : Int) : List[Byte] = {
    source_ip_addr ++ destination_ip_addr ++
    parse_hex("00 11") ++ split_word(length)  // protocol == 0x0011 == udp
  }
}

class UdpDuplex(val ppp_duplex : AbstractDuplex,
                val our_ip_addr : List[Byte],
                val our_port : Int,
                val remote_ip_addr : List[Byte],
                val remote_port : Int) {
  // IP id counter has 2 bytes, its initial value should be random
  private var id_counter : Int = (math.random * 40000).toInt

  def name = ppp_duplex.name

  def get_id : Int = {
    id_counter += 1
    id_counter
  }

  def write_udp(data : List[Byte]) : Unit = {
    val udp_datagram = new UdpDatagram(our_port, remote_port, data)
    val ip_datagram = 
      new IpDatagram(get_id, our_ip_addr, remote_ip_addr, udp_datagram)
    val ip_data = new IpData(ip_datagram.bytes)

    ppp_duplex.write_ppp(new PppFrame(Protocol.IP, ip_data))
  }

  def read_udp(timeout_millis : Long) : Either[String, List[Byte]] = {
    val frame = ppp_duplex.read_ppp(timeout_millis) 
    if(frame.protocol == Protocol.Timeout)
      Left("Timeout")
    else if(frame.protocol != Protocol.IP)
      Left("Not IP protocol")
    else {
      val ip_data = frame.payload.asInstanceOf[IpData].bytes
      val data_inside_ip = IpDatagram.unpack(ip_data)
      if(data_inside_ip.isLeft)
        Left(data_inside_ip.left.get)
      else 
        UdpDatagram.unpack(data_inside_ip.right.get,
                           our_ip_addr, remote_ip_addr)
    }
  }
}


// ==============================
// WTP and WSP protocols
// ==============================

sealed abstract class WtpPduType
object WtpPduType {
  object Invoke extends WtpPduType
  object Ack extends WtpPduType
  object Result extends WtpPduType
  object SegmentedInvoke extends WtpPduType
  object Nak extends WtpPduType
  object Unknown extends WtpPduType
}

class WtpHeader(val pdu_type : WtpPduType,
                val tid : Int,
                val ttr : Boolean,
                val transaction_class : Int,
                val packet_sequence_number : Int) {
  import WtpHeader._

  private def bool_to_int(bool : Boolean) : Int = {
    if(bool) 1 else 0
  }

  def bytes : List[Byte] = {
    val pdu_type_int = pdu_type_values.map(_.swap).apply(pdu_type)
    // other fields are zero and we are not concerned to it for now
    val first_byte = ((pdu_type_int << 3) | (bool_to_int(ttr) << 1)).toByte
    if(pdu_type == WtpPduType.Ack) 
      first_byte +: split_word(tid)
    else {
      val last_byte = 
        if(pdu_type == WtpPduType.Invoke)
          transaction_class.toByte
        else if(pdu_type == WtpPduType.SegmentedInvoke)
          packet_sequence_number.toByte
        else 
          throw new InvalidComposeException   
      first_byte +: split_word(tid) :+ last_byte      
    }
  }
}

object WtpHeader {
  import WtpPduType._
  val pdu_type_values = Map[Int, WtpPduType](0x01 -> Invoke,
                                             0x02 -> Result,
                                             0x03 -> Ack,
                                             0x05 -> SegmentedInvoke,
                                             0x07 -> Nak)

  def get_wtp_pdu_type(bytes : List[Byte]) : WtpPduType = {
    val pdu_type_int = (bytes(0) & 0x78) >>> 3
    pdu_type_values.getOrElse(pdu_type_int, Unknown)
  }

  def get_tid(bytes : List[Byte]) : Int = {
    byte_list_to_int(bytes.slice(1,3)) & 0x7F
  }

  def get_ttr(bytes : List[Byte]) : Boolean = 
    ((bytes.head & 0x02) != 0)

  def get_psn(bytes : List[Byte]) : Int = {
    if(get_wtp_pdu_type(bytes) != SegmentedInvoke)
      0
    else
      bytes(3).toInt
  }

  def get_rid(bytes : List[Byte]) : Boolean = 
    ((bytes.head & 0x01) != 0)

  def make_wtp_invoke_fragments(
    tid : Int, wsp_pdu : List[Byte]) : List[List[Byte]] = {
    val wsp_fragments = 
      wsp_pdu.grouped(wtp_max_transmit_unit_size).toList
    val first_wtp_pdu_header = 
      new WtpHeader(WtpPduType.Invoke, tid, false, 2, 0)
    val first_wtp_pdu = first_wtp_pdu_header.bytes ++ wsp_fragments.head
    // 上面的这个wtp的pdu的确是invoke，但是与函数make_wtp_invoke_pdu
    // 中不同，因为GTR与TTR两个位都清零了，表示后面有其它的分组，也就
    // 是其它的fragment。

    // 这个make_wtp_more_pdus可能返回一个空表。它对frag_list中的每一
    // 个都包上一个wtp的头部，设置好psn序列号。一律设置为后面有其它的
    // 分组。
    def make_wtp_more_pdus(wsp_frag_list : List[List[Byte]],
                           psn : Int) : List[List[Byte]] 
    // psn = packet sequence number
    = {
      if(wsp_frag_list.isEmpty)
        List[List[Byte]]()
      else {
        val wtp_pdu_header = 
          new WtpHeader(WtpPduType.SegmentedInvoke, tid, false, 0, psn)
        val wtp_pdu = wtp_pdu_header.bytes ++ wsp_frag_list.head
        wtp_pdu :: make_wtp_more_pdus(wsp_frag_list.tail, psn + 1)
      }           
    }

    val raw_wtp_pdu_list : List[List[Byte]] = 
      first_wtp_pdu :: make_wtp_more_pdus(wsp_fragments.tail, 1)
    // raw_wtp_pdu_list所有的分组都指示后面有其它的分组，包括最后一个。
    // 所以，最后一个分组要调整。这最后一个也可能根本就是第一个，也就
    // 是说前后一共只有一个分组。下面的代码能够正确地处理这些情况。
    val last_wtp_pdu = raw_wtp_pdu_list.last
    val last_wtp_pdu_with_signal = 
      (last_wtp_pdu.head | 0x02).toByte :: last_wtp_pdu.tail
    // 以上这是给最后一个分组（可能是第一个invoke，也可能是第二个开始以
    // 后的segmented invoke）设置TTR位。
    val wtp_pdu_list = 
      raw_wtp_pdu_list.init :+ last_wtp_pdu_with_signal

    wtp_pdu_list //这个返回的表就可以直接用udp打包后发送出了。
  }
}

sealed abstract class WspPduType 
object WspPduType {
  object Connect extends WspPduType
  object ConnectReply extends WspPduType
  object Reply extends WspPduType
  object Disconnect extends WspPduType
  object Post extends WspPduType
  object Unknown extends WspPduType

  val pdu_type_values = Map[Byte, WspPduType](0x01.toByte -> Connect,
                                              0x02.toByte -> ConnectReply,
                                              0x04.toByte -> Reply,
                                              0x05.toByte -> Disconnect,
                                              0x60.toByte -> Post)

  def value_of(pdu_type : WspPduType) : Byte = {
    pdu_type_values.map(_.swap).apply(pdu_type)
  }
}

object Wsp {
  def get_wsp_pdu_type(bytes : List[Byte]) : WspPduType = {
    val wtp_pdu_type = WtpHeader.get_wtp_pdu_type(bytes)
    val typebyte = 
      if(wtp_pdu_type == WtpPduType.Invoke)
        bytes(4)
      else
        bytes(3)
    WspPduType.pdu_type_values.getOrElse(typebyte, WspPduType.Unknown)
  }

  def make_wsp_mms_post_pdu(data : List[Byte]) : List[Byte] = {
    val uri = mmsc_url.getBytes.toList
    val content_type = 
      "application/vnd.wap.mms-message".getBytes.toList :+ 0x00.toByte

    (WspPduType.value_of(WspPduType.Post) ::
     int_to_uintvar(uri.length) ++
     int_to_uintvar(content_type.length) ++
     uri ++
     content_type ++
     data)
  }

  // uintvar是wap系列协议中规定的，可变长的int值，字节的最高位为1表示
  // uintvar串未结束，最高位为0表示是uintvar串的最后一个字节。
  def get_first_uintvar(data : List[Byte]) : List[Byte] = {
    val prefix = data.takeWhile( x => (x & 0x80) != 0 ) // 所有最高位为1的
    prefix :+ data(prefix.length) // 第一个最高位为0的字节
  }

  // 根据wap系列系列协议规定，将数字进行8位转7位的转换，留出最高位作为
  // “连续指示位”。
  def int_to_uintvar(number : Int) : List[Byte] = {
    def recur(num : Int, seq : List[Byte]) : List[Byte] = 
      if(num == 0)
        seq
      else
        recur(num >> 7, ((num & 0x7f) | 0x80).toByte :: seq)    

    recur(number >> 7, List((number & 0x7f).toByte))
  }

  // 最高位是“连续指示位”，在这里可以忽略，因为get_first_uintvar已经确
  // 定了uintvar在何处结束。
  def uintvar_to_int(sequence : List[Byte]) : Int = {
    def recur(num : Int, seq : List[Byte]) : Int = 
      if(seq.isEmpty)
        num
      else
        recur((num << 7) | (seq.head & 0x7f), seq.tail)

    recur(0, sequence)
  }
}

sealed abstract class WspState
object WspState {
  object Closed extends WspState
  object ConnectReqSent extends WspState
  object Connected extends WspState
  object DataSent extends WspState
}

class WspAutomaton(val duplex : UdpDuplex) {
  sealed abstract class WspEvent
  object WspEvent {
    object OpenConnection extends WspEvent
    object ConnectReplyRcvd extends WspEvent
    object CloseConnection extends WspEvent
    object SendMms extends WspEvent
    object ReplyRcvd extends WspEvent
    object AckRcvd extends WspEvent
    object Nak extends WspEvent
    object SoftTimeout extends WspEvent
    object HardTimeout extends WspEvent
  }

  import Wsp._
  import WspState._
  import WspEvent._
  
  abstract class WspAction { def perform : Unit }

  private var transaction_id : Int = 0

  def get_tid : Int = {
    transaction_id += 1
    transaction_id
  }

  // if due to bug, we did not record a session id, then 
  // this default value is used in a closing request
  private var session_id : List[Byte] = int_to_uintvar(100000)
  private var retransmit_counter : Int = max_retransmit_times
  private var recent_sent_request : List[Byte] = null
  private var received_tid : Int = 0
  private var the_state : WspState = Closed
  private var data_to_send : List[Byte] = null
  private var recent_fragments : List[List[Byte]] = null
  private var missing_psn_list : List[Byte] = null
  def state = the_state
  def set_state(s : WspState) : Unit = { the_state = s } // for testing

  val no_op_action = new WspAction { def perform : Unit = {} }
  val hard_timeout_action = new WspAction {
    def perform : Unit = {
      the_state = Closed
      throw new SessionTimeoutException
    }
  }
  
  val state_transition_table = Map(
    (Closed, OpenConnection) -> new WspAction { def perform : Unit = {
      initialize_retransmit_counter
      send_connect_invoke
      the_state = ConnectReqSent
      read_from_remote
    }},
    (ConnectReqSent, ConnectReplyRcvd) -> new WspAction { def perform : Unit = {
      send_ack
      the_state = Connected
    }},
    (ConnectReqSent, SoftTimeout) -> new WspAction { def perform : Unit = {
      val request = 
        (recent_sent_request.head | 0x01).toByte :: recent_sent_request.tail
      duplex.write_udp(request)
      retransmit_counter -= 1
      read_from_remote
    }},
    (Connected, CloseConnection) -> new WspAction { def perform : Unit = {
      send_disconnect_invoke
      the_state = Closed
      // according to protocol specification, there would not be any acks
    }},
    (Connected, SendMms) -> new WspAction { def perform : Unit = {
      if(data_to_send != null) {
        initialize_retransmit_counter
        send_data
        the_state = DataSent
        read_from_remote
      }
    }},
    (DataSent, ReplyRcvd) -> new WspAction { def perform : Unit = {
      send_ack
      the_state = Connected
    }},
    (DataSent, SoftTimeout) -> new WspAction { def perform : Unit = {
      // we do not retransmit, or may send duplicate messages due to 
      // too late response of remote.
      retransmit_counter -= 1
      read_from_remote
    }},
    (DataSent, AckRcvd) -> new WspAction { def perform : Unit = {
      read_from_remote  // nothing special
    }},
    (DataSent, Nak) -> new WspAction { def perform : Unit = {
      resend_data
      read_from_remote
    }}
  )

  def read_from_remote : Unit = {
    val response = duplex.read_udp(request_timeout_interval)
    
    if(response.isLeft) {
      if(response.left.get == "Timeout") {
        if(retransmit_counter > 0)
          feed_event(SoftTimeout)
        else
          feed_event(HardTimeout)
      } else {
        Log.error(duplex.name, response.left.get)
        read_from_remote // silently discard
      }
    } 
    else {
      val data = response.right.get
      if(WtpHeader.get_wtp_pdu_type(data) == WtpPduType.Result &&
         Wsp.get_wsp_pdu_type(data) == WspPduType.ConnectReply) {
           received_tid = WtpHeader.get_tid(data)
           session_id = get_first_uintvar(data.drop(4))
           feed_event(ConnectReplyRcvd)
         }
      else if(WtpHeader.get_wtp_pdu_type(data) == WtpPduType.Result &&
              Wsp.get_wsp_pdu_type(data) == WspPduType.Reply) {
         received_tid = WtpHeader.get_tid(data)
         feed_event(ReplyRcvd)
      }
      else if(WtpHeader.get_wtp_pdu_type(data) == WtpPduType.Nak) {
        val missing_psn_count = data(3)
        missing_psn_list = data.drop(4).take(missing_psn_count)
        feed_event(Nak)
      }
      else if(WtpHeader.get_wtp_pdu_type(data) == WtpPduType.Ack) {
        feed_event(AckRcvd) 
      }
      else {
        Log.warn_unhandled_packet(duplex.name, data)
        read_from_remote // silently discard
      }
    }
  }

  def feed_event(event : WspEvent) : Unit = {
    Log.debug(duplex.name, event.toString)
    val action = if(event == HardTimeout) {
      hard_timeout_action
    } else if(!(state_transition_table contains (state, event))) {
      no_op_action
    } else {
      state_transition_table(state, event)
    }

    action.perform
  }

  def connect : Unit = {
    feed_event(OpenConnection)
  }

  def disconnect : Unit = {
    feed_event(CloseConnection)
  }

  def send_mms(data : List[Byte]) : Unit = {
    data_to_send = data
    feed_event(SendMms)
  }

  def initialize_retransmit_counter : Unit = {
    retransmit_counter = max_retransmit_times
  }

  def send_connect_invoke : Unit = {
    val wtp_header = new WtpHeader(WtpPduType.Invoke, get_tid, true, 2, 0)
    
    val s_sdu_size_hex = 
      0x81.toByte :: int_to_uintvar(wsp_server_sdu_size_capability)
    val c_sdu_size_hex = 
      0x80.toByte :: int_to_uintvar(wsp_client_sdu_size_capability)
    val capabilities = 
      ((c_sdu_size_hex.length.toByte :: c_sdu_size_hex) ++
       (s_sdu_size_hex.length.toByte :: s_sdu_size_hex))        
    val wsp_connect_pdu = 
      (WspPduType.value_of(WspPduType.Connect) ::
       0x10.toByte ::  // version = 1.0 (0x10)
       int_to_uintvar(capabilities.length) ++ // capabilities length
       List(0x00.toByte) ++ // headers length
       capabilities)

    val whole_packet = wtp_header.bytes ++ wsp_connect_pdu
    recent_sent_request = whole_packet
    duplex.write_udp(whole_packet)
  }

  def send_ack : Unit = {
    val wtp_header = new WtpHeader(
      WtpPduType.Ack, received_tid, true, 0, 0)
    val whole_packet = wtp_header.bytes
    duplex.write_udp(whole_packet)
  }

  def send_disconnect_invoke : Unit = {
    val wtp_header = new WtpHeader(
      WtpPduType.Invoke, get_tid, true, 0, 0)
    val wsp_pdu = WspPduType.value_of(WspPduType.Disconnect) :: session_id
    val whole_packet = wtp_header.bytes ++ wsp_pdu
    duplex.write_udp(whole_packet)
  }

  def send_data : Unit = {
    val wsp_pdu = Wsp.make_wsp_mms_post_pdu(data_to_send)
    val fragments = WtpHeader.make_wtp_invoke_fragments(get_tid, wsp_pdu)
    recent_fragments = fragments

    fragments.foreach(duplex.write_udp)
  }

  def resend_data : Unit = {
    missing_psn_list.foreach(i => {
      val fragment = recent_fragments(i)
      val new_fragment = (fragment.head | 0x01).toByte :: fragment.tail
      duplex.write_udp(new_fragment)
    })
  }
}

