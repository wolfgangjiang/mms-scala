package com.edoctor.mms

object SessionHelpers {
  // 这里的escape，指的是ppp协议规定的0x7d escape。之所以要有这样一个
  // escape，是因为ppp协议是基于可打印符号的，而且以0x7e为帧的分隔符。
  // 所以，不可打印的控制字符，以及0x7e，都不允许出现在普通数据中，一旦
  // 普通数据确实有这样的字节（普通数据是二进制数据，任何字节都可能
  // 有），就必须加以转义（escape）。转义的规则是，将字节与0x20相异或，
  // 然后在前面加上0x7d，表示该字节已经被转义。如果数据本身中有一个字节
  // 是0x7d，该字节也要转义成为0x7d 0x5d两个字节。详见rfc 1662标准。
  def decode_0x7d_escape(list : List[Byte]) : List[Byte] = {
    if(list.isEmpty)
      List[Byte]()
    else if(list.head == 0x7d && list.tail.nonEmpty) 
      (list.tail.head ^ 0x20).toByte :: decode_0x7d_escape(list.tail.tail)
    else 
      list.head :: decode_0x7d_escape(list.tail)
  }

  // 参见函数decode_escape
  def encode_0x7d_escape(list : List[Byte]) : List[Byte] = {
    def needs_escape(data : Byte) : Boolean = 
      ((data.toInt & 0xFF) < 0x20) || (List(0x7e, 0x7d, 0x91, 0x93) contains data) 
    // 上面这里的toInt是因为不这样做的话，0xAB这样的会被认为是负数而小
    // 于0x20，这样虽然也会被远端正确解码，但是不必要的转义会严重浪费带
    // 宽。

    if(list.isEmpty)
      List[Byte]()
    else if(needs_escape(list.head))
      0x7d.toByte :: (list.head ^ 0x20).toByte :: encode_0x7d_escape(list.tail)
    else
      list.head :: encode_0x7d_escape(list.tail)
  }

  def to_hex_string(data : List[Byte]) : String = 
    data.map(x => String.format("%02X", new java.lang.Byte(x))).mkString(" ")

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
  // 示长度，高位多出的字节会被丢弃，返回的整数只表现出参数里的最后四个
  // 字节的内容。
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
    if(ip_list.length != 4)
      throw new MalformedIpAddressException
    ip_list
  }

  // parse_ip的逆运算，将二进制串表示的ip地址转化为“用点号分隔的数字”字
  // 符串。
  def ip_to_string(ip_list : List[Byte]) : String = {
    assert(ip_list.length == 4)
    ip_list.map(x => (x.toInt & 0xFF)).mkString(".")
  }


  // 这个校验和不是循环冗余，而是ip、tcp、udp所使用的奇偶校验和。这段代
  // 码差不多是从网上的相关既有代码中改写来的。
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
}
import SessionHelpers._

object SessionParameters {
  // 每隔burst_read_interval读取一次远端数据，而不是一直空循环等待，这
  // 是为了省出cpu时间
  val burst_read_interval = 10L

  // request_timeout仅用于config-request和terminal-request的应答超时判
  // 定，以毫秒为单位。
  val request_timeout_interval : Long = 5*1000L 

  val max_retransmit_times = 3

  val our_lcp_config_req_options = parse_hex("02 06 00 00 00 00")

  val our_pap_auth_req_info = parse_hex("00 00")

  val wap_proxy_ip_addr = "10.0.0.172"
  val wap_proxy_port = 9201
  val our_port = 2048
  // 我们的ip地址是动态获取的

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
