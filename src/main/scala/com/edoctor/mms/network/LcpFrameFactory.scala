package com.edoctor.mms.network

import com.edoctor.mms.Parameters

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Jan 5, 2011
 * Time: 5:09:45 PM
 * To change this template use File | Settings | File Templates.
 */

object LcpPacketFactory {

  /**
   * create a lcp config req ppp packet 
   */
  def req_frame: LcpFrame = {
    val req = Parameters.ourLcpConfigReqOptions
    val size = req.length + 4
    val sizeByteLow = (size & 0xFF).toByte
    val sizeByteHigh = ((size >> 8) & 0xFF).toByte
    LcpFrame(0x01, Parameters.getNewIdCounter :: sizeByteHigh :: sizeByteLow :: req)
  }

  def ack_frame(req: PppPacket) = {
    LcpFrame(0x02, req.data.drop(1))
  }
}

case class Frame(protocol_id_high: Byte, protocol_id_low: Byte, code: Byte, data: List[Byte]) {
  def to_bytes: List[Byte] = {
    code :: data
  }

  def to_ppp_packet: PppPacket = {
    PppPacket(protocol_id_high, protocol_id_low, to_bytes)
  }
}
case class LcpFrame(override val code: Byte, override val data: List[Byte])
        extends Frame(LcpFrame.protocal_id_high, LcpFrame.protocal_id_low, code, data)

object LcpFrame {
  val protocal_id_high: Byte = 0xc0.toByte
  val protocal_id_low: Byte = 0x21.toByte
}
