package com.edoctor.mms
import org.apache.commons.net.telnet.TelnetClient
import java.io.InputStream
import java.io.OutputStream
import net.tambur.mms.MMMessage

import net.tambur.mms.MMConstants
import java.io.File
import java.io.DataInputStream
import java.io.FileInputStream

/*
object App {
  val phones = Map("姜节汇" -> "13122747605",
                   "王北南" -> "13501800943",
                   "项国林" -> "15900673748",
                   "吴愈" -> "13818696816",
                   "李煜春" -> "18601665964",
                   "褚红明" -> "13816465191",
                   "周子予" -> "18602142887",
                   "仓平" -> "13918814481",
                   "李晨" -> "13761616139",
                   "刘小杰" -> "13482134924",
                   "徐佳伟" -> "13661503473",
                   "张君君" -> "13671736062",
                   "邹倚鸣" -> "13564316828",
                   "李红旺" -> "15000479872")
               
  def makeMmsMessage : List[Byte] = {
    val msg = new MMMessage()
    msg.setMessageType(MMConstants.MESSAGE_TYPE_M_SEND_REQ)
    msg.setTransactionId("0001")
    msg.setFrom("+8615000279445/TYPE=PLMN")
    msg.setTo("+86" + phones("姜节汇") + "/TYPE=PLMN")
    //  msg.setTo("+8613122747605/TYPE=PLMN")
    //  msg.setTo("+8613501800943/TYPE=PLMN")
    msg.setSubject(" 彩信测试") // 第一个字符不能是汉字，可以是空格。
    msg.setVersion(1)
    msg.setContentType("application/vnd.wap.multipart.related")
    

    val fImage = new File("a.gif")
    val data = new Array[Byte](fImage.length.toInt)
    val dis = new DataInputStream(new FileInputStream(fImage))
    dis.readFully(data)
    dis.close()
    msg.addPart("image/gif", data, false, null, null)

    val text_data = "您好，这是测试彩信，祝您健康快乐。".getBytes("utf-8")
    //  val text_data = "现在彩信可以同时发送文字、音乐和图片，到达率很高。文字可以很长。标题和内容都可以使用中文。君不见黄河之水天上来⑵，奔流到海不复回。　　君不见高堂明镜悲白发，朝如青丝暮成雪⑶。　　人生得意须尽欢⑷，莫使金樽空对月。　　天生我材必有用，千金散尽还复来。　　烹羊宰牛且为乐，会须一饮三百杯⑸。　　岑夫子，丹丘生⑹，将进酒，杯莫停⑺。　　与君歌一曲⑻，请君为我倾耳听。　　钟鼓馔玉不足贵⑽，但愿长醉不复醒。　　古来圣贤皆寂寞，惟有饮者留其名。　　陈王昔时宴平乐，斗酒十千恣欢谑⑿。　　主人何为言少钱⒀，径须沽取对君酌⒁。　　五花马⒂，千金裘，　　呼儿将出换美酒，与尔同销万古愁⒃。[1]".getBytes
    msg.addPart("text/plain; charset=\"utf-8\"", text_data, false, null, null)

    val f_midi = new File("a.mid")
    val midi_data = new Array[Byte](f_midi.length.toInt)
    val midi_dis = new DataInputStream(new FileInputStream(f_midi))
    midi_dis.readFully(midi_data)
    midi_dis.close()
    msg.addPart("audio/midi; autostart=true", midi_data, false, null, null)

    msg.encode.toList
  }


  def main(args : Array[String]) : Unit = {
    println("Hello world.")
    val session1 = new MmsSession("192.168.10.230", 964, makeMmsMessage)
    session1.start
    while(session1.getState != Thread.State.TERMINATED) {} // wait
    if(!session1.is_successful) {
      println("=================================================")
      val session2 = new MmsSession("192.168.10.230", 964, makeMmsMessage)
      session2.start
    }
  }
}

*/

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
  val P_wsp_connect_reply = Value
  val P_wsp_reply_success = Value
  val P_wsp_reply_fail = Value
}

import PacketKindDefinitions._

object SessionHelpers {
  def list_to_hex(list : List[Byte]) : String = 
    list.map(x => String.format("%02X", new java.lang.Byte(x))).mkString(" ")

  def println_hex(data : List[Byte]) : Unit = 
    println(list_to_hex(data))

  def decode_escape(list : List[Byte]) : List[Byte] = {
    if(list.isEmpty)
      List[Byte]()
    else if(list.head == 0x7d)
      (list.tail.head ^ 0x20).toByte :: decode_escape(list.tail.tail)
    else 
      list.head :: decode_escape(list.tail)
  }

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

  private val fcs_table_16 = calculate_fcs_table_16
  
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

  def attach_fcs16(data : List[Byte]) : List[Byte] = {
    val fcs = get_fcs16(data)
    val fcsLowOctet = (fcs & 0xFF).toByte
    val fcsHighOctet = ((fcs >> 8) & 0xFF).toByte
    data ++ List(fcsLowOctet, fcsHighOctet)
  }

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

  def parse_hex(str : String) : List[Byte] = 
    str.split("\\s+").toList.filterNot(_ == "").map(x => Integer.parseInt(x, 16).toByte)

  def split_word(word : Int) : List[Byte] = 
    List(((word >> 8) & 0xFF).toByte, (word & 0xFF).toByte)

  def split_double_word(doubleWord : Int) : List[Byte] = 
    split_word((doubleWord >> 16) & 0xFFFF) :::
    split_word(doubleWord & 0xFFFF)

  def byte_list_to_int(list : List[Byte]) : Int = {
    def recur(list : List[Byte], number : Int) : Int =
      if(list.isEmpty)
        number
      else
        recur(list.tail, (number << 8) + (list.head.toInt & 0xFF))

    recur(list, 0)
  }

  def parse_ip(ip_str : String) : List[Byte] = {
    val ip_list = ip_str.split("\\.").toList.map(s => (s.toInt & 0xFF).toByte)
    assert(ip_list.length == 4)
    ip_list
  }

  def ip_to_string(ip_list : List[Byte]) : String = {
    assert(ip_list.length == 4)
    ip_list.map(x => (x.toInt & 0xFF)).mkString(".")
  }
  
  def parse_packet(data : List[Byte]) : Packet = {
    val unknown_report = new Packet(
      P_unknown_packet, Map("data" -> list_to_hex(data)))
    val corrupted_report = new Packet(
      P_corrupted_packet, Map("data" -> list_to_hex(data)))

    if(data.length < 6)
      return corrupted_report
    
    if(data.slice(0,2) != List(0xFF, 0x03).map(_.toByte))
      return unknown_report

    if(!is_fcs16_good(data))
      return corrupted_report

    byte_list_to_int(data.slice(2,4)) match {
      case 0xC021 =>
        if(data.length < 10)
          corrupted_report
        else data(4).toInt match {
          case 0x01 => 
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
      case 0xC023 =>
        if(data.length < 6)
          corrupted_report
        else data(4).toInt match {
          case 0x02 =>
            new Packet(P_pap_auth_ack,
                       Map("id" -> data(5)))
          case _ => unknown_report
        }          
      case 0x8021 =>
        if(data.length < 14)
          corrupted_report
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
      case 0x0021 =>
        // ppp frame wrapper = 6 bytes, ip header = 20 bytes
        // udp header = 8 bytes, wtp header = 3+ bytes
        if(data.length < 37)
          corrupted_report
        else {          
          val wtp_pdu = data.drop(32).dropRight(2) 
          val tid = (byte_list_to_int(wtp_pdu.slice(1,3)) & 0x7F)
          ((wtp_pdu(0) >> 3) & 0x0F).toInt match {
            case 0x02 => // wtp result
              if(wtp_pdu.length < 7)
                corrupted_report
              else wtp_pdu(3).toInt match {
                case 0x02 => 
                  new Packet(P_wsp_connect_reply,
                             Map("tid" -> tid,
                                 "session_id" ->
                                 get_first_uintvar(wtp_pdu.drop(4))))
                case 0x04 =>
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
            case _ =>
              unknown_report
          }
        }
      case _ => unknown_report
    }
  } // parse_packet

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
          print("received: ")
          println_hex(decode_escape(data.reverse))
          return parse_packet(decode_escape(data.reverse))
        }
        // else do nothing with an empty packet and wait for next
        last_received_time = System.currentTimeMillis
      }
      Thread.sleep(burst_read_interval)
    }
    new Packet(P_timeout, Map.empty)
  }

  def warn_unhandled_packet(packet : Packet) : Unit = {
    println("warning of unhandled packet: " + packet)
  }

  def ppp_encapsulate(data : List[Byte]) = 
    (0x7e.toByte +: 
     encode_escape(
       attach_fcs16(0xFF.toByte :: 0x03.toByte :: data))
     :+ 0x7e.toByte)

  def send_packet(s_info : SessionInfo, packet : List[Byte]) = {
    s_info.out_s.write(packet.toArray)
    s_info.out_s.flush
    print("sent: ")
    println_hex(decode_escape(packet))
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
      print("retranmit:")
      println_hex(decode_escape(packet))
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

class SessionInfo(my_in_s : InputStream, my_out_s : OutputStream) {
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
            warn_unhandled_packet(packet)        
        case _ =>
          warn_unhandled_packet(packet)
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
            warn_unhandled_packet(packet)
        case _ =>
          warn_unhandled_packet(packet)
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
            warn_unhandled_packet(packet)
        case _ =>
          warn_unhandled_packet(packet)
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
          } else warn_unhandled_packet(packet)            
        case Packet(P_ipcp_config_ack, param) => 
          if(param("id") == our_second_config_req_id)
            assigned_ip_addr = param("ip_addr").asInstanceOf[List[Byte]]
          else
            warn_unhandled_packet(packet)
        case _ =>
          warn_unhandled_packet(packet)
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
          else warn_unhandled_packet(packet)            
        case _ =>
          warn_unhandled_packet(packet)
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
    
    while(timeout_counter > 0) {
      val packet = read_packet(s_info)
      packet match {
        case Packet(P_wtp_ack, param) =>
          if(param("tid") == tid)
            {} // do nothing
          else
            warn_unhandled_packet(packet)
        case Packet(P_wsp_reply_success, param) =>
          if(param("tid") == tid)
            return // 成功
          else
            warn_unhandled_packet(packet)
        case Packet(P_wsp_reply_fail, param) =>
          throw new SessionException("Bad reply from mmsc")
        case Packet(P_timeout, param) => {
          println("waiting ...")
          timeout_counter -= 1
        }
        case _ =>
          warn_unhandled_packet(packet)
      }
    }
    throw new SessionException
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
      tc.connect(modem_ip, modem_port)
      val s_info = new SessionInfo(tc.getInputStream, tc.getOutputStream)
      if(dial(s_info)) {
        try {
          SessionHandlers.lcp_connect(s_info)
          SessionHandlers.pap_authenticate(s_info)
          SessionHandlers.ipcp_configure(s_info)
          println("My Ip Address: " + s_info.ip_addr)
          try {
            SessionHandlers.wsp_connect(s_info)
            SessionHandlers.send_mms(s_info, msg_data)
            success = true
            println("**********************")
            println("***    Success!    ***")
            println("**********************")
          } finally {
            SessionHandlers.wsp_disconnect(s_info)
          }
        } finally {
          SessionHandlers.lcp_terminate(s_info)
        }
      } else throw new SessionException("No carrier")
    } catch {
      case e : SessionException =>
        println("session exception: " + e)
      case e : java.net.ConnectException =>
        println("connect exception: " + e)
    } finally {
      tc.disconnect
    }    
  }
}
