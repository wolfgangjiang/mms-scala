package com.edoctor.mms.actors

import java.io.{InputStream, OutputStream}
import org.apache.commons.net.telnet.TelnetClient
import actors.{TIMEOUT, Actor}
import net.lag.logging.Logger
import com.edoctor.mms.network._

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Jan 5, 2011
 * Time: 10:50:53 AM
 * To change this template use File | Settings | File Templates.
 */

class ModemSession(modem_ip: String, port: Int) extends Actor {
  val log = Logger.get

  def act = create_telnet_session

  def create_telnet_session: Unit = {
    val tc = new TelnetClient
    try {
      tc.connect(modem_ip, port)
      log.info("telnet connected")
      create_ppp_session(tc.getInputStream, tc.getOutputStream)
    } catch {
      case e: RuntimeException => //any exception ,restart telnet_session
        log.error(e, "runtime exception")
        //TODO: cut ppp connection
        tc.disconnect
        log.debug("telnet disconnected, restart")
        //TODO: clear mail box???
        create_telnet_session
      case e =>
        log.trace(e, "other exception")
        throw e
    }
    log.info("create_telnet_session finished")
  }


  def create_ppp_session(in: InputStream, out: OutputStream): Unit = {
    log.debug("atd dial up start")
    out.write("ATD*99***1#\r".getBytes)
    out.flush
    log.info("atd dial up finish")
    Thread.sleep(5000)
    val package_recipient = new PppRecipient(in, this)
    package_recipient.start
    process_lcp(out)
  }

  def process_lcp(out: OutputStream): Unit = {
    log.info("start lcp process")
    log.debug("send lcp config request")
    send_packet(LcpPacketFactory.req_frame.to_ppp_packet, out)
    wait_lcp_ack
    def wait_lcp_ack: Unit = reactWithin(30000) {
      case p: PppPacket if (p.data.head == 0x02) => //ack received
        log.info("configAck received, process pap")
        Thread.sleep(5000)
        process_pap(out)
      case p: PppPacket if (p.data.head == 0x01) => //req received
        log.info("configReq received, sending a configAck")
        send_packet(LcpPacketFactory.ack_frame(p).to_ppp_packet, out)
        wait_lcp_ack
      case TIMEOUT =>
        log.info("configReq timeout, sending a configReq")
        send_packet(LcpPacketFactory.req_frame.to_ppp_packet, out)
        wait_lcp_ack
    }
  }

  def process_pap(out: OutputStream): Unit = {
    log.info("start pap process")
    send_packet(PapFrameFactory.auth_req_frame.to_ppp_packet, out)
    reactWithin(15000) {
      case PppPacket(_, 0x23, data) if (data.head == 0x02) =>
        log.info("pap act received, start ipcp")
        process_ipcp(out)
      case TIMEOUT =>
        log.debug("restart pap processing: waiting pap ack timeout")
        process_pap(out)
      case _ =>
        log.debug("restart pap processing: other packet received")
        process_pap(out)
    }
  }

  def process_ipcp(out: OutputStream):Unit = {
    log.info("start ipcp process")
    Thread.sleep(3000)
    send_packet(IpcpFrameFactory.ipcp_config_req.to_ppp_packet, out)
    reactWithin(15000) {
      case p:PppPacket =>
        p match {
          case PppPacket(IpcpFrame.protocal_id_high, IpcpFrame.protocal_id_low, 0x01::rest) => //ipcp req
            log.info("ipcp req received and sending an ipcp ack")
            send_packet(IpcpFrameFactory.ipcp_config_ack(p).to_ppp_packet, out)
            process_ipcp(out)
          case PppPacket(IpcpFrame.protocal_id_high, IpcpFrame.protocal_id_low, 0x02::rest) => //ipcp nak
            log.info("ipcp ack received")
            process_ip(p.data.slice(6,10))
          case PppPacket(IpcpFrame.protocal_id_high, IpcpFrame.protocal_id_low, 0x03::rest) => //ipcp nak
            log.info("ipcp nak received")
            send_packet(IpcpFrameFactory.ipcp_config_req(p).to_ppp_packet, out)  //send req again with the given ip
            process_ipcp(out)
          case _ => //ignore other packets
            process_ipcp(out)
        }
      case _ => //ignore the messages in other types
        process_ipcp(out)
    }
  }

  def process_ip(ip: List[Byte]) = {
    log.info("ip address granted: " + ip.toString)
  }
  private def send_packet(packet: PppPacket, out: OutputStream) = {
    log.trace("send out ppp packet:" + packet.to_print_string)
    out.write(packet.to_raw_bytes.toArray)
    out.flush
  }
}