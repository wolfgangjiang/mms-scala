package com.edoctor.mms.network

import com.edoctor.mms.Parameters

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Jan 6, 2011
 * Time: 6:07:33 PM
 * To change this template use File | Settings | File Templates.
 */

object IpcpFrameFactory {
  def ipcp_config_req:IpcpFrame = {
    ipcp_config_req(Parameters.ourIpcpInitialReqOptions)

  }

  def ipcp_config_req(nak: PppPacket):IpcpFrame = {
    ipcp_config_req(0x03.toByte :: 0x06.toByte :: nak.data.slice(6,10))
  }

  def ipcp_config_ack(req: PppPacket) = {
    IpcpFrame(0x02, req.data.tail)
  }
  private def ipcp_config_req(req: List[Byte]):IpcpFrame = {
    val size = req.length + 4
    val sizeByteLow = (size & 0xFF).toByte
    val sizeByteHigh = ((size >> 8) & 0xFF).toByte
    val req_with_size = sizeByteHigh :: sizeByteLow :: req
    IpcpFrame(0x01, Parameters.getNewIdCounter :: req_with_size)
  }
}
case class IpcpFrame (override val code: Byte, override val data: List[Byte])
        extends Frame(IpcpFrame.protocal_id_high, IpcpFrame.protocal_id_low, code, data)

object IpcpFrame {
  val protocal_id_high: Byte = 0x80.toByte
  val protocal_id_low: Byte = 0x21.toByte
}