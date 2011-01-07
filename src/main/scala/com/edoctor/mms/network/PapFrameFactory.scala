package com.edoctor.mms.network

import com.edoctor.mms.Parameters

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Jan 6, 2011
 * Time: 1:40:26 PM
 * To change this template use File | Settings | File Templates.
 */

object PapFrameFactory {
  def auth_req_frame: PapFrame = {
    var req = Parameters.ourAuthReq
    val size = req.length + 4
    val sizeByteLow = (size & 0xFF).toByte
    val sizeByteHigh = ((size >> 8) & 0xFF).toByte
    req = sizeByteHigh :: sizeByteLow :: req
    PapFrame(0x01, Parameters.getNewIdCounter :: req)
  }

}

case class PapFrame (override val code: Byte, override val data: List[Byte]) extends Frame(0xc0.toByte, 0x23, code, data)