package com.edoctor.mms
import org.apache.commons.net.telnet.TelnetClient
import java.io.InputStream
import java.io.OutputStream
import net.tambur.mms.MMMessage

import net.tambur.mms.MMConstants
import java.io.File
import java.io.DataInputStream
import java.io.FileInputStream


object SessionParameters {
  // receive_timeout_interval用于read_packet，如果超时就返回一个
  // timeout_packet
  val receive_timeout_interval : Long = 2 * 1000L
  // 每隔burst_read_interval读取一次远端数据，而不是一直空循环等待，这
  // 是为了省出cpu时间
  val burst_read_interval : Long = 10L
  // request_timeout仅用于config-request和terminal-request的应答超时判
  // 定，也就是说仅仅用于Retransmitter对象。以毫秒为单位。
  val request_timeout_interval : Long = 15*1000L 

  val our_lcp_config_req_options = 
    List(0x02, 0x06, 0x00, 0x00, 0x00, 0x00).map(_.toByte)

  val our_pap_auth_req_info = 
    List(0x00, 0x00).map(_.toByte)

  val our_ipcp_initial_req_options = 
    List(0x03, 0x06, 0x00, 0x00, 0x00, 0x00).map(_.toByte)

  val max_retransmit_times = 3

  val wap_proxy_ip_addr = "10.0.0.172"
  val wap_proxy_port = 9201
  val our_port = 2048
  // 我们的ip地址是动态获取的，储存在SessionInfo对象中。
  val default_ip_ttl = 0xFF.toByte // time to live of ip datagram sent

  // 以下两个sdu size capability应大于一条彩信的最大长度，即应大于50k。
  // sdu是service data unit的缩写，在wsp中不分组，所以可以设得很大。在
  // wtp中才分组。sdu基本上是wsp里的payload的概念，一个sdu中包含一个完
  // 整的短信。
  val wsp_client_sdu_size_capability = 60 * 1024 // 接收彩信用
  val wsp_server_sdu_size_capability = 100 * 1024 // 发送彩信用
  // 把wtp分组设得较小，就可以避免ip协议中的进一步分组。
  // 若ip包的长度小于576就一定不会被进一步分组了。
  val wtp_max_transmit_unit_size = 400 // wtp分组时一组最大长度

  val mmsc_url = "http://mmsc.monternet.com"
}

import SessionParameters._


object PacketKindDefinitions extends Enumeration{
  type PacketKind = Value

  // 以下P_xxx是所有可选的枚举值，它们必须由一个大写字母开头，否则在
  // case子句中会被认为是被临时绑定的变量名，而不是供匹配用的常量名。
  val P_timeout = Value
  val P_corrupted_packet = Value
  val P_unknown_packet = Value
  val P_lcp_config_req = Value 
  val P_lcp_config_ack = Value
  val P_lcp_terminate_ack = Value
  val P_pap_auth_ack = Value
  val P_ipcp_config_req = Value
  val P_ipcp_config_ack = Value
  val P_ipcp_config_nak = Value
  val P_wtp_ack = Value
  val P_wtp_nak = Value
  val P_wsp_connect_reply = Value
  val P_wsp_reply_success = Value
  val P_wsp_reply_fail = Value
}

import PacketKindDefinitions._

object SessionHelpers {
  // 将List[Byte]表示的二进制串，转换为十六进制格式的字符串，供阅读和打
  // 印。
  def list_to_hex(list : List[Byte]) : String = 
    list.map(x => String.format("%02X", new java.lang.Byte(x))).mkString(" ")

  def println_hex(data : List[Byte]) : Unit = 
    println(list_to_hex(data))

  // 这里的escape，指的是ppp协议规定的0x7d escape。之所以要有这样一个
  // escape，是因为ppp协议是基于可打印符号的，而且以0x7e为帧的分隔符。
  // 所以，不可打印的控制字符，以及0x7e，都不允许出现在普通数据中，一旦
  // 普通数据确实有这样的字节（普通数据是二进制数据，任何字节都可能
  // 有），就必须加以转义（escape）。转义的规则是，将字节与0x20相异或，
  // 然后在前面加上0x7d，表示该字节已经被转义。如果数据本身中有一个字节
  // 是0x7d，该字节也要转义成为0x7d 0x5d两个字节。详见rfc 1662标准。
  def decode_escape(list : List[Byte]) : List[Byte] = {
    if(list.isEmpty)
      List[Byte]()
    else if(list.head == 0x7d)
      (list.tail.head ^ 0x20).toByte :: decode_escape(list.tail.tail)
    else 
      list.head :: decode_escape(list.tail)
  }

  // 参见函数decode_escape
  def encode_escape(list : List[Byte]) : List[Byte] = {
    def needs_escape(data : Byte) : Boolean = 
      (data < 0x20) || (List(0x7e, 0x7d, 0x91, 0x93) contains data)

    if(list.isEmpty)
      List[Byte]()
    else if(needs_escape(list.head))
      0x7d.toByte :: (list.head ^ 0x20).toByte :: encode_escape(list.tail)
    else
      list.head :: encode_escape(list.tail)
  }

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
    
    fcs == 0xf0b8
  }

  // 这里可以用split_word重写
  def attach_fcs16(data : List[Byte]) : List[Byte] = {
    val fcs = get_fcs16(data)
    val fcsLowOctet = (fcs & 0xFF).toByte
    val fcsHighOctet = ((fcs >> 8) & 0xFF).toByte
    data ++ List(fcsLowOctet, fcsHighOctet)
  }

  // 这个校验和不是循环冗余，而是ip、tcp、udp所使用的奇偶校验和。这段代
  // 码差不多也是从网上的相关既有代码中改写来的。
  def compute_checksum(data : List[Byte]) : Int = {
    def get_head_num(data : List[Byte]) : Int = 
      if(data.length == 1)
        (data.head & 0xFF) << 8
      else
        ((data.head & 0xFF) << 8) + (data.tail.head & 0xFF)

    def recur(number : Int, data : List[Byte]) : Int = 
      if(data.isEmpty)
        number
      else {
        val simpleSum = number + get_head_num(data)
        recur((simpleSum >> 16) + (simpleSum & 0xFFFF), data.drop(2))
      }
    
    (~recur(get_head_num(data), data.drop(2)) & 0xFFFF) // bits inversed
  }

  // 将一段形如"15 0A C0"的十六进制字符串转化为它所表示的二进制串。这样
  // 就给了我们手动输入一段二进制串的方便手段。
  def parse_hex(str : String) : List[Byte] = 
    str.split("\\s+").toList.filterNot(_ == "").map(x => Integer.parseInt(x, 16).toByte)

  // 将一个两字节的int整数，转化为等价的2个二进制字节，高位在左，低位在
  // 右，与tcp/ip协议数据包头部的规定相同。如果int整数超过0xFFFF，则超
  // 过的部分被丢弃。
  def split_word(word : Int) : List[Byte] = 
    List(((word >> 8) & 0xFF).toByte, (word & 0xFF).toByte)

  // 将一个四字节的int整数，转化为等价的4个二进制字节，高位在左，低位在
  // 右，与tcp/ip协议数据包头部的规定相同。
  def split_double_word(doubleWord : Int) : List[Byte] = 
    split_word((doubleWord >> 16) & 0xFFFF) :::
    split_word(doubleWord & 0xFFFF)

  // 这是split_word和split_double_word的逆运算，处理所有的字节，无论多
  // 长。如果溢出也不作处理，所以一旦超过4个字节，也就是超过int的java表
  // 示长度，就会产生错误的结果。
  def byte_list_to_int(list : List[Byte]) : Int = {
    def recur(list : List[Byte], number : Int) : Int =
      if(list.isEmpty)
        number
      else
        recur(list.tail, (number << 8) + (list.head.toInt & 0xFF))

    recur(list, 0)
  }

  // 将形如"192.168.10.51"的字符串转化为二进制串。如果长度不是4个字节，
  // 会抛出异常。
  def parse_ip(ip_str : String) : List[Byte] = {
    val ip_list = ip_str.split("\\.").toList.map(s => (s.toInt & 0xFF).toByte)
    assert(ip_list.length == 4)
    ip_list
  }

  // parse_ip的逆运算，将二进制串表示的ip地址转化为“用点号分隔的数字”字
  // 符串。
  def ip_to_string(ip_list : List[Byte]) : String = {
    assert(ip_list.length == 4)
    ip_list.map(x => (x.toInt & 0xFF)).mkString(".")
  }

  // 本函数非常长。它接受一个去掉了头尾0x7e标识、处理好了转义的ppp包，
  // 分析里面是什么东西。有什么办法把它写得优雅一些呢？从某个角度看，它
  // 很难简化，因为它本身表示的是复杂的、人为的协议标准，而不是简洁优雅
  // 的数学逻辑。
  def parse_packet(data : List[Byte]) : Packet = {
    val unknown_report = new Packet(
      P_unknown_packet, Map("data" -> list_to_hex(data)))
    val corrupted_report = new Packet(
      P_corrupted_packet, Map("data" -> list_to_hex(data)))

    if(data.length < 6)  // ppp包，头部4个字节，尾部2个字节校验和。
      return corrupted_report // 太短的就不合法。
    
    if(data.slice(0,2) != List(0xFF, 0x03).map(_.toByte))
      return unknown_report // ppp包的开头两个字节必然是FF 03。

    if(!is_fcs16_good(data))  // 循环冗余校验
      return corrupted_report

    // ppp包的第3、4个字节是协议标识
    byte_list_to_int(data.slice(2,4)) match {  
      case 0xC021 =>  // C0 21表示LCP协议
        if(data.length < 10)  // LCP包必须有类型、长度和option
          corrupted_report // 这样处理可以避免数组越界异常
        else data(4).toInt match {
          case 0x01 =>  // Packet对象的第一个参数表示了它的类型。
            new Packet(P_lcp_config_req, 
                       Map("id" -> data(5), 
                           "options" -> data.drop(8).dropRight(2)))
          case 0x02 =>
            new Packet(P_lcp_config_ack,
                       Map("id" -> data(5)))
          case 0x06 =>
            new Packet(P_lcp_terminate_ack,
                       Map("id" -> data(5)))
          case _ => unknown_report
        }
      case 0xC023 =>  // C0 23表示PAP认证协议
        if(data.length < 6)
          corrupted_report
        else data(4).toInt match {
          case 0x02 =>
            new Packet(P_pap_auth_ack,
                       Map("id" -> data(5)))
          case _ => unknown_report
        }          
      case 0x8021 =>  // 80 21表示IPCP协议，为建立IP通讯作准备
        if(data.length < 14)  // 如果数据包里不包含某个ip地址，
          corrupted_report  // 那我们就不关心它，即便它不是corrupted，
                            // 也是unknown的
        else {
          val param = Map("id" -> data(5),
                          "ip_addr" -> data.slice(10,14))
          data(4).toInt match {
            case 0x01 =>
              new Packet(P_ipcp_config_req, param)
            case 0x02 =>
              new Packet(P_ipcp_config_ack, param)
            case 0x03 =>
              new Packet(P_ipcp_config_nak, param)
            case _ => unknown_report
          }
        }
      case 0x0021 => // 00 21表示IP协议，网络层的主力协议
        // ppp frame wrapper = 6 bytes, ip header = 20 bytes
        // udp header = 8 bytes, wtp header = 3+ bytes
        if(data.length < 37) // 我们仅仅考虑里面是UDP和WTP、WSP协议的情况
          corrupted_report  // 其它情况对我们来说无意义。
        else {          
          val wtp_pdu = data.drop(32).dropRight(2) 
          val tid = (byte_list_to_int(wtp_pdu.slice(1,3)) & 0x7F)
          ((wtp_pdu(0) >> 3) & 0x0F).toInt match {
            case 0x02 => // wtp result
              if(wtp_pdu.length < 7)
                corrupted_report
              else wtp_pdu(3).toInt match { // 这个字节是wsp类型
                case 0x02 => 
                  new Packet(P_wsp_connect_reply,
                             Map("tid" -> tid,
                                 "session_id" ->
                                 get_first_uintvar(wtp_pdu.drop(4))))
                case 0x04 => // wsp reply
                  if(wtp_pdu(4) == 0x20)
                    new Packet(P_wsp_reply_success, 
                               Map("tid" -> tid))
                  else
                    new Packet(P_wsp_reply_fail, Map.empty())
                case _ =>
                  unknown_report
              }
            case 0x03 =>
              new Packet(P_wtp_ack, 
                         Map("tid" -> tid))
            case 0x07 =>  // 对重传的请求，对包里给出的psn（序列号）都要重传
              new Packet(P_wtp_nak,
                         Map("tid" -> tid, 
                             "psn" -> wtp_pdu.drop(4).take(wtp_pdu(3))))
            case _ =>
              unknown_report
          }
        }
      case _ => unknown_report
    }
  } // parse_packet

  // 从输入流中取得一个完整的ppp frame。如果在规定的timeout时间内无法取
  // 得，就返回一个P_timeout类型的packet。这里的packet在以后是否应该都
  // 改名叫frame？
  def read_packet(s_info : SessionInfo) : Packet = {
    var octet : Byte = 0
    var data = List[Byte]()
    var last_received_time = System.currentTimeMillis

    while(System.currentTimeMillis - last_received_time < 
          receive_timeout_interval) {
      if(s_info.in_s.available > 0) {
        octet = s_info.in_s.read.toByte
        if(octet != 0x7e)
          data = octet :: data
        else if(!data.isEmpty) {
          Log.bytes(s_info.name, 
                    "received: " + list_to_hex(decode_escape(data.reverse)))
          return parse_packet(decode_escape(data.reverse))
        }
        // else do nothing with an empty packet and wait for next
        last_received_time = System.currentTimeMillis
      }
      Thread.sleep(burst_read_interval)
    }
    new Packet(P_timeout, Map.empty)
  }

  def warn_unhandled_packet(s_info : SessionInfo, packet : Packet) : Unit = {
    Log.error(s_info.name, 
              "warning of unhandled packet: " + packet)
  }

  def ppp_encapsulate(data : List[Byte]) = 
    (0x7e.toByte +: 
     encode_escape(
       attach_fcs16(0xFF.toByte :: 0x03.toByte :: data))
     :+ 0x7e.toByte)

  def send_packet(s_info : SessionInfo, data : List[Byte]) = {
    s_info.out_s.write(data.toArray)
    s_info.out_s.flush
    Log.bytes(s_info.name, 
              "sent: " + list_to_hex(decode_escape(data)))
  }

  def dial(s_info : SessionInfo) : Boolean = {
    // 发送拨号信息（这是假定sim卡已经配置好拨号前的准备）

    s_info.out_s.write("ATD*99***1#\r".getBytes)
    s_info.out_s.flush

    Thread.sleep(1000)

    var lastReceivedTime = System.currentTimeMillis

    while(System.currentTimeMillis - lastReceivedTime < Parameters.timeOutMillis) {
      if(s_info.in_s.available > 0) {
        lastReceivedTime = System.currentTimeMillis
        if(s_info.in_s.read == 0x7e)
          return true
      }
    }

    return false
  } 

  def compute_udp_checksum(s_info : SessionInfo, 
                           datagram : List[Byte]) : Int = {
    val pseudo_header = 
      (parse_ip(s_info.ip_addr) :::
       parse_ip(wap_proxy_ip_addr) :::
       split_word(0x0011) :::
       split_word(datagram.length))

    compute_checksum(pseudo_header ::: datagram)
  }

  def fill_in_udp_checksum(s_info : SessionInfo, 
                           datagram : List[Byte]) : List[Byte] = {
    val checksum = compute_udp_checksum(s_info, datagram)
    datagram.take(6) ::: split_word(checksum) ::: datagram.drop(8)
  }

  def fill_in_ip_header_checksum(header : List[Byte]) : List[Byte] = {
    val checksum = compute_checksum(header)
    header.take(10) ::: split_word(checksum) ::: header.drop(12)
  }

  def udp_encapsulate(s_info : SessionInfo, 
                      payload : List[Byte]) : List[Byte] = {
    val udp_datagram = fill_in_udp_checksum(s_info,
      split_word(our_port) ::: // source port
      split_word(wap_proxy_port) ::: // destination port
      split_word(8 + payload.length) :::  // datagram length
      split_word(0) :::  // checksum to be filled in later
      payload)

    val ip_header = fill_in_ip_header_checksum(
      split_word(0x4500) :::  // version, header len, "type of service"
      split_word(20 + 8 + payload.length) ::: // total length
      split_word(s_info.get_new_ip_id) ::: // identifier of datagram
      split_word(0) ::: // we won't make fragments
      List(default_ip_ttl, 0x11.toByte) ::: // ttl, protocol = udp = 0x11
      split_word(0) ::: // dummy checksum to be filled in later
      parse_ip(s_info.ip_addr) ::: // our ip addr
      parse_ip(wap_proxy_ip_addr)) // destination ip addr

    ppp_encapsulate(split_word(0x0021) ::: 
      ip_header ::: udp_datagram)
  }

  def get_first_uintvar(data : List[Byte]) : List[Byte] = {
    val prefix = data.takeWhile( x => (x & 0x80) != 0 )
    prefix :+ data(prefix.length)
  }

  def int_to_uintvar(number : Int) : List[Byte] = {
    def recur(num : Int, seq : List[Byte]) : List[Byte] = 
      if(num == 0)
        seq
      else
        recur(num >> 7, ((num & 0x7f) | 0x80).toByte :: seq)    

    recur(number >> 7, List((number & 0x7f).toByte))
  }

  def uintvar_to_int(sequence : List[Byte]) : Int = {
    def recur(num : Int, seq : List[Byte]) : Int = 
      if(seq.isEmpty)
        num
      else
        recur((num << 7) | (seq.head & 0x7f), seq.tail)

    recur(0, sequence)
  }
}

import SessionHelpers._

class Retransmitter(packet : List[Byte], s_info : SessionInfo) {
  var last_sent_time = System.currentTimeMillis
  var counter = max_retransmit_times

  def visit : Unit = {
    if(System.currentTimeMillis - last_sent_time > request_timeout_interval) {
      Log.bytes(s_info.name, 
                "retransmit: " + list_to_hex(decode_escape(packet)))
      s_info.out_s.write(packet.toArray)
      s_info.out_s.flush
      last_sent_time = System.currentTimeMillis
      counter -= 1
      if(counter <= 0)
        throw new SessionException("Retransmitted too many times")
    }
  }
}

case class Packet(kind : PacketKind, parameters : Map[String, Any])

class SessionInfo(my_name : String, my_in_s : InputStream, my_out_s : OutputStream) {
  val name = my_name
  val in_s = my_in_s
  val out_s = my_out_s
  var ip_addr = ""
  var session_id = List[Byte]()

  private var ppp_id_counter = 0x10
  def get_new_ppp_id : Byte = {
    ppp_id_counter += 1
    if(ppp_id_counter > 0x20)
      ppp_id_counter = 0x10
    ppp_id_counter.toByte
  }

  private var ip_id_counter = (math.random * 40000).toInt
  def get_new_ip_id : Int = {
    ip_id_counter += 1
    ip_id_counter
  }

  // wtp的tid与ppp的共用计数器，似乎效果还不坏。
  def get_new_wtp_tid : Int = {
    get_new_ppp_id.toInt & 0xFF
  }
}

class SessionException(msg : String) extends RuntimeException {
  def this() = {
    this("")
  }
}

object SessionHandlers {
  def lcp_connect(s_info : SessionInfo) : Unit = {
    val our_config_req_id = s_info.get_new_ppp_id

    def make_lcp_config_req : List[Byte] = 
      ppp_encapsulate(
        0xC0.toByte :: 0x21.toByte ::
        0x01.toByte :: our_config_req_id ::
        split_word(our_lcp_config_req_options.length + 4) :::
        our_lcp_config_req_options)

    def make_lcp_config_ack(param : Map[String, Any]) : List[Byte] = 
      ppp_encapsulate(
        0xC0.toByte :: 0x21.toByte ::
        0x02.toByte :: param("id").asInstanceOf[Byte] ::
        split_word(param("options").asInstanceOf[List[Byte]].length + 4) :::
        param("options").asInstanceOf[List[Byte]])

    val our_config_req = make_lcp_config_req
    var config_req_rcvd = false
    var config_ack_rcvd = false

    send_packet(s_info, our_config_req)
    val retransmitter = new Retransmitter(our_config_req, s_info)
    while(!(config_req_rcvd && config_ack_rcvd)) {
      val packet = read_packet(s_info)
      packet match {
        case Packet(P_lcp_config_req, param) => {
          send_packet(s_info, make_lcp_config_ack(param))
          config_req_rcvd = true
        }
        case Packet(P_lcp_config_ack, param) => 
          if(param("id") == our_config_req_id)
            config_ack_rcvd = true
          else
            warn_unhandled_packet(s_info, packet)        
        case _ =>
          warn_unhandled_packet(s_info, packet)
      }
      retransmitter.visit // maybe perform retransmitting 
    }
  }

  def lcp_terminate(s_info : SessionInfo) : Unit = {
    val our_terminate_req_id = s_info.get_new_ppp_id

    def make_lcp_terminate_req : List[Byte] = 
      ppp_encapsulate(
        0xC0.toByte :: 0x21.toByte ::
        0x05.toByte :: our_terminate_req_id ::
        0x00.toByte :: 0x04.toByte :: Nil) // length is fixed

    val our_terminate_req = make_lcp_terminate_req
    var terminate_ack_rcvd = false

    send_packet(s_info, our_terminate_req)
    val retransmitter = new Retransmitter(our_terminate_req, s_info)
    while(!terminate_ack_rcvd) {
      val packet = read_packet(s_info)
      packet match {
        case Packet(P_lcp_terminate_ack, param) =>
          if(param("id") == our_terminate_req_id)
            terminate_ack_rcvd = true
          else
            warn_unhandled_packet(s_info, packet)
        case _ =>
          warn_unhandled_packet(s_info, packet)
      }
      retransmitter.visit
    }
  }

  def pap_authenticate(s_info : SessionInfo) : Unit = {
    val our_auth_req_id = s_info.get_new_ppp_id

    def make_pap_auth_req : List[Byte] = 
      ppp_encapsulate(
        0xC0.toByte :: 0x23.toByte ::
        0x01.toByte :: our_auth_req_id :: 
        split_word(our_pap_auth_req_info.length + 4) :::
        our_pap_auth_req_info)

    val our_auth_req = make_pap_auth_req
    var auth_ack_rcvd = false

    send_packet(s_info, our_auth_req)
    val retransmitter = new Retransmitter(our_auth_req, s_info)
    while(!auth_ack_rcvd) {
      val packet = read_packet(s_info)
      packet match {
        case Packet(P_pap_auth_ack, param) =>
          if(param("id") == our_auth_req_id)
            auth_ack_rcvd = true
          else
            warn_unhandled_packet(s_info, packet)
        case _ =>
          warn_unhandled_packet(s_info, packet)
      }
      retransmitter.visit
    }
  }

  def ipcp_configure(s_info : SessionInfo) : Unit = {
    val our_first_config_req_id = s_info.get_new_ppp_id
    val our_second_config_req_id = s_info.get_new_ppp_id

    // ipcp config req的长度固定为10字节，因为ip地址的长度固定为4字节。
    def make_ipcp_config_req(id : Byte, ip_addr : List[Byte]) : List[Byte] =
      ppp_encapsulate(
        0x80.toByte :: 0x21.toByte ::
        0x01.toByte :: id ::
        0x00.toByte :: 0x0A.toByte :: // fixed length = 10
        0x03.toByte :: 0x06.toByte ::
        ip_addr)

    def make_ipcp_config_ack(param : Map[String, Any]) : List[Byte] =
      ppp_encapsulate(
        0x80.toByte :: 0x21.toByte ::
        0x02.toByte :: param("id").asInstanceOf[Byte] ::
        split_word(param("ip_addr").asInstanceOf[List[Byte]].length + 6) :::
        0x03.toByte :: 0x06.toByte :: 
        param("ip_addr").asInstanceOf[List[Byte]])
        
    val our_first_config_req = 
      make_ipcp_config_req(our_first_config_req_id, 
                           List(0,0,0,0).map(_.toByte))
    var our_second_config_req = List[Byte]()    

    var assigned_ip_addr = List[Byte]()
    var config_req_rcvd = false

    send_packet(s_info, our_first_config_req)
    var retransmitter = new Retransmitter(our_first_config_req, s_info)
    while(assigned_ip_addr.isEmpty || !config_req_rcvd) {
      val packet = read_packet(s_info)
      packet match {
        case Packet(P_ipcp_config_req, param) => {
          send_packet(s_info, make_ipcp_config_ack(param))
          config_req_rcvd = true
        }
        case Packet(P_ipcp_config_nak, param) => 
          if(param("id") == our_first_config_req_id) {
            our_second_config_req = make_ipcp_config_req(
              our_second_config_req_id,
              param("ip_addr").asInstanceOf[List[Byte]])
            send_packet(s_info, our_second_config_req)
            retransmitter = new Retransmitter(our_second_config_req, s_info)
            // retransmitter for first config request is discarded here
            // and will never be visited in future
          } else warn_unhandled_packet(s_info, packet)            
        case Packet(P_ipcp_config_ack, param) => 
          if(param("id") == our_second_config_req_id)
            assigned_ip_addr = param("ip_addr").asInstanceOf[List[Byte]]
          else
            warn_unhandled_packet(s_info, packet)
        case _ =>
          warn_unhandled_packet(s_info, packet)
      } // match
      retransmitter.visit // maybe perform retransmitting
    } // while
    s_info.ip_addr = ip_to_string(assigned_ip_addr)
  }

  def wsp_connect(s_info : SessionInfo) : Unit = {
    val connect_tid = s_info.get_new_wtp_tid
    def make_invoke_connect_pdu : List[Byte] = {
      val wtp_header = 0x0A.toByte +: split_word(connect_tid) :+ 0x02.toByte
      // 上面0x0A的意思是：CON = 0, PDU type = 0x01 = invoke,
      // GTR = 0, TTR = 0, RID = 0; 而0x02的意思是：Version = 0b00,
      // TIDnew = 0, U/P = 0, TCL = 0b10 = 0x02。

      val s_sdu_size_hex = 
        0x81.toByte :: int_to_uintvar(wsp_server_sdu_size_capability)
      val c_sdu_size_hex = 
        0x80.toByte :: int_to_uintvar(wsp_client_sdu_size_capability)
      val capabilities = 
        ((c_sdu_size_hex.length.toByte :: c_sdu_size_hex) :::
         (s_sdu_size_hex.length.toByte :: s_sdu_size_hex))        
      val wsp_connect_pdu = 
        (split_word(0x0110) ::: // pdutype=connect(0x01),version=1.0(0x10)
         int_to_uintvar(capabilities.length) ::: // capabilities length
         List(0x00.toByte) ::: // headers length
         capabilities)

      udp_encapsulate(s_info, wtp_header ::: wsp_connect_pdu)
    }

    def make_ack_pdu(param : Map[String, Any]) : List[Byte] = 
      udp_encapsulate(s_info, 
        0x18.toByte :: split_word(param("tid").asInstanceOf[Int]))
      // 上面0x18的意思是：CON = 0, PDU type = 0x11 = acknowledge,
      // Tve/Tok = 0, RID = 0


    val our_connect_invoke = make_invoke_connect_pdu
    var connect_reply_rcvd = false

    send_packet(s_info, our_connect_invoke)
    var retransmitter = new Retransmitter(our_connect_invoke, s_info)
    while(!connect_reply_rcvd) {
      val packet = read_packet(s_info)
      packet match {
        case Packet(P_wsp_connect_reply, param) =>
          if(param("tid") == connect_tid) {
            send_packet(s_info, make_ack_pdu(param))
            s_info.session_id = param("session_id").asInstanceOf[List[Byte]]
            connect_reply_rcvd = true
          } 
          else warn_unhandled_packet(s_info, packet)            
        case _ =>
          warn_unhandled_packet(s_info, packet)
      }
      retransmitter.visit // maybe perform retransmitting
    }
  }

  def wsp_disconnect(s_info : SessionInfo) : Unit = {
    val disconnect_tid = s_info.get_new_wtp_tid
    def make_invoke_disconnect_pdu : List[Byte] = {
      val wtp_header = 
        0x0A.toByte +: split_word(disconnect_tid) :+ 0x00.toByte
      // 上面0x0A的意思是：CON = 0, PDU type = 0x01 = invoke,
      // GTR = 0, TTR = 0, RID = 0; 而0x02的意思是：Version = 0b00,
      // TIDnew = 0, U/P = 0, TCL = 0b00 = 0x00。

      val wsp_disconnect_pdu = 
        (0x05.toByte :: // pdutype = disconnect(0x05)
         s_info.session_id)

      udp_encapsulate(s_info, wtp_header ::: wsp_disconnect_pdu)
    }

    send_packet(s_info, make_invoke_disconnect_pdu)
    // 不会有应答
  }

  def send_mms(s_info : SessionInfo, msg_data : List[Byte]) : Unit = {
    val tid = s_info.get_new_wtp_tid
    // 这个tid是transaction id，也即是序列号的意思。分组的多个wtp
    // packets拥有同一个tid。

    def make_wsp_mms_post_pdu : List[Byte] = {
      val uri = mmsc_url.getBytes.toList
      val content_type = 
        "application/vnd.wap.mms-message".getBytes.toList :+ 0x00.toByte

      (0x60.toByte :: // 表示是post类型的wsp pdu
       int_to_uintvar(uri.length) :::
       int_to_uintvar(content_type.length) :::
       uri :::
       content_type :::
       msg_data)
    }

    def make_wtp_invoke_fragments(wsp_pdu : List[Byte]) : List[List[Byte]] = {
      val wsp_fragments = 
        wsp_pdu.grouped(wtp_max_transmit_unit_size).toList
      val first_wtp_pdu_header = 
        0x08.toByte +: split_word(tid) :+ 0x02.toByte
      val first_wtp_pdu = first_wtp_pdu_header ::: wsp_fragments.head
      // 上面的这个wtp的pdu的确是invoke，但是与函数make_wtp_invoke_pdu中
      // 不同，因为GTR与TTR两个都清零了，表示后面有其它的分组。

      def make_wtp_more_pdus(wsp_frag_list : List[List[Byte]],
                             psn : Int) : List[List[Byte]] 
      // psn = packet sequence number
      = {
        if(wsp_frag_list.isEmpty)
          List[List[Byte]]()
        else {
          val wtp_pdu_header = 
            0x28.toByte +: split_word(tid) :+ psn.toByte
          val wtp_pdu = wtp_pdu_header ::: wsp_frag_list.head
          wtp_pdu :: make_wtp_more_pdus(wsp_frag_list.tail, psn + 1)
        }           
      }

      val raw_wtp_pdu_list : List[List[Byte]] = 
        first_wtp_pdu :: make_wtp_more_pdus(wsp_fragments.tail, 1)
      val last_wtp_pdu = raw_wtp_pdu_list.last
      val last_wtp_pdu_with_signal = 
        (last_wtp_pdu.head | 0x02).toByte :: last_wtp_pdu.tail
      // 以上这是给最后一个分组（可能是第一个invoke，也可能是第二个开始以
      // 后的segmented invoke）设置TTR位。
      val wtp_pdu_list = 
        raw_wtp_pdu_list.init :+ last_wtp_pdu_with_signal

      wtp_pdu_list
    }

    val wtp_fragments = 
      make_wtp_invoke_fragments(make_wsp_mms_post_pdu)

    def send_one_fragment(wtp_pdu : List[Byte]) : Unit =
      send_packet(s_info, udp_encapsulate(s_info, wtp_pdu))

    wtp_fragments.foreach(send_one_fragment)
    var timeout_counter = 30
    
    def resend(fragment : List[Byte]) : Unit = {
      val new_fragment = (fragment.head & 0x01).toByte :: fragment.tail
      // set RID = 1, which means this pdu is being retransmitted
      send_one_fragment(new_fragment)
    }

    while(timeout_counter > 0) {
      val packet = read_packet(s_info)
      packet match {
        case Packet(P_wtp_ack, param) =>
          if(param("tid") == tid)
            {} // do nothing
          else
            warn_unhandled_packet(s_info, packet)
        case Packet(P_wtp_nak, param) =>
          if(param("tid") == tid) {
            param("psn").asInstanceOf[List[Byte]].foreach { 
              psn => resend(wtp_fragments(psn)) }
          } 
          else 
            warn_unhandled_packet(s_info, packet)
        case Packet(P_wsp_reply_success, param) =>
          if(param("tid") == tid)
            return // 成功
          else
            warn_unhandled_packet(s_info, packet)
        case Packet(P_wsp_reply_fail, param) =>
          throw new SessionException("Bad reply from mmsc")
        case Packet(P_timeout, param) => {
          // Log.info(s_info.name, "waiting ...")
          timeout_counter -= 1
        }
        case _ =>
          warn_unhandled_packet(s_info, packet)
      }
    }
    throw new SessionException("Waited too long after sending all mms data and still cannot get wsp acknowledge.")
  }
}

class MmsSession(modem_ip : String, 
                 modem_port : Int,
                 msg_data : List[Byte]) extends Thread {
  private var success = false

  def is_successful = this.success

  override def run : Unit = {
    val tc = new TelnetClient
    try {
      try {
        tc.connect(modem_ip, modem_port)
        val s_info = new SessionInfo(
          modem_ip + ":" + modem_port.toString, 
          tc.getInputStream, tc.getOutputStream)
        if(dial(s_info)) {
          try {
            SessionHandlers.lcp_connect(s_info)
            SessionHandlers.pap_authenticate(s_info)
            SessionHandlers.ipcp_configure(s_info)
            Log.info(s_info.name, "My Ip Address: " + s_info.ip_addr)
            try {
              SessionHandlers.wsp_connect(s_info)
              SessionHandlers.send_mms(s_info, msg_data)
              success = true
              Log.info(s_info.name, " ******* Success! ******* ")
            } finally {
              SessionHandlers.wsp_disconnect(s_info)
            }
          } finally {
            SessionHandlers.lcp_terminate(s_info)
          }
        } else throw new SessionException("No carrier")
      } catch {
        case e : SessionException => 
          Log.error(modem_ip + ":" + modem_port.toString, e)
        case e : java.net.ConnectException => 
          Log.error(modem_ip + ":" + modem_port.toString, e)
      } finally {
        tc.disconnect
      }    
    } catch {
      case e : InterruptedException => 
        Log.error(modem_ip + ":" + modem_port.toString, 
                  "Interrupted")
      case e : Exception =>
        Log.error(modem_ip + ":" + modem_port.toString, e)
    } finally {
      try {
        tc.disconnect
      } catch {
        case e : Exception => { }
      }
    }
  }
}
