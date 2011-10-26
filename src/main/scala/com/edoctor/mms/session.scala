package com.edoctor.mms

class MmsSession(modem_ip : String,
                 modem_port : Int,
                 msg_data : List[Byte]) extends Thread with ActualRemote {
  private var success = false
  
  def is_successful = this.success

  // 发送彩信的全过程有10个步骤。
  override def run : Unit = {
    try { // 1. 建立telnet连接
      with_telnet_connection(modem_ip, modem_port) { duplex => {
        send(duplex, msg_data)
      }} // 10. 解除telnet连接
    } catch {
      case e : InterruptedException =>
        Log.error(modem_ip + ":" + modem_port.toString,
                  "Interrupted")
      case e : Exception =>
        Log.error(modem_ip + ":" + modem_port.toString, e)
    }
  }

  private def send(duplex : AbstractDuplex, 
                   msg_data : List[Byte]) : Unit = {
    var lcp_automaton : LcpAutomaton = null
    var pap_automaton : PapAutomaton = null
    var ipcp_automaton : IpcpAutomaton = null
    var wsp_automaton : WspAutomaton = null
    try {
      dial(duplex) // 2. 拨号
      lcp_automaton = new LcpAutomaton(duplex) 
      lcp_automaton.open // 3.建立lcp连接
      pap_automaton = new PapAutomaton(duplex) 
      pap_automaton.authenticate // 4. pap验证
      ipcp_automaton = new IpcpAutomaton(duplex)
      val our_ip_addr = ipcp_automaton.get_ip // 5.ipcp交互
      Log.info(duplex.name, 
               "My Ip Address: " + SessionHelpers.ip_to_string(our_ip_addr))
      val udp_duplex = new UdpDuplex(
        duplex,
        our_ip_addr,
        SessionParameters.our_port,
        SessionParameters.wap_proxy_ip_addr,
        SessionParameters.wap_proxy_port)      
      try {
        wsp_automaton = new WspAutomaton(udp_duplex)
        wsp_automaton.connect  // 6. 建立wsp连接
        wsp_automaton.send_mms(msg_data)  // 7. 发送彩信
        success = true
        Log.info(duplex.name, " ******* Success! ******* ")        
      } finally {
        wsp_automaton.disconnect // 8. 解除wsp连接
      }
    } finally {
      lcp_automaton.close // 9.解除lcp连接
    }
  }

  private def dial(duplex : AbstractDuplex) : Unit = {
    duplex.say_text("ATD*99***1#")
    val response = duplex.listen_text(500L)
    if(!(response containsSlice "CONNECT")) {
      Log.error(duplex.name, "dial failed")
      throw new DialFailedException
    }
  }
}
