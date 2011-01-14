package com.edoctor.mms

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 1:35:17 PM
 * To change this template use File | Settings | File Templates.
 */

object Parameters {
  val mmscatIP = "192.168.10.230"
  val mmscatPort = 966
  val timeOutMillis = 30 * 1000L
  val ourConfigReqInterval = 3 * 1000L

  private var idCounter = 0x10

  def getNewIdCounter : Byte = {
    idCounter += 1
    if(idCounter > 0x20)
      idCounter = 0x10
    idCounter.toByte
  }

  val   ourLcpConfigReqOptions =
    List(0x02, 0x06, 0x00, 0x00, 0x00, 0x00).map(_.toByte)

  val ourIpcpInitialReqOptions =
    List(0x03, 0x06, 0x00, 0x00, 0x00, 0x00).map(_.toByte)

  val ourAuthReq =
    List(0x00, 0x00).map(_.toByte)
}