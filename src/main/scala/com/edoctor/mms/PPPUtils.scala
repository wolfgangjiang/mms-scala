package com.edoctor.mms

import java.io.{InputStream, OutputStream}
import network.FCSCalculator
import org.apache.commons.net.telnet.TelnetClient
import ParseUtils._

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 1:02:56 PM
 * To change this template use File | Settings | File Templates.
 */

object PPPUtils {
  // 拨号，见到输入流中有0x7e即返回true。不过在它返回时，这个0x7e已经被它
  // 读到，不会被后续的read()再读到了。如果一直没有收到0x7e，则等待对方服
  // 务器厌烦，也就是连续若干秒钟没有收到任何数据后，返回false。返回true
  // 表示可以进行下一步的ppp连接处理，返回false表示ppp连接不可用。
  def beforeFirstFlag(in_s: InputStream, out_s: OutputStream): Boolean = {
    // 发送拨号信息（这是假定sim卡已经配置好拨号前的准备

    out_s.write("ATD*99***1#\r".getBytes)
    out_s.flush

    Thread.sleep(1000)

    var lastReceivedTime = System.currentTimeMillis

    while (System.currentTimeMillis - lastReceivedTime < Parameters.timeOutMillis) {
      if (in_s.available > 0) {
        lastReceivedTime = System.currentTimeMillis
        if (in_s.read == 0x7e)
          return true
      }
    }

    return false
  }

  def testPPPconnection(): Unit = {
  val tc = new TelnetClient

  tc.connect(Parameters.mmscatIP, Parameters.mmscatPort)

  if(beforeFirstFlag(tc.getInputStream, tc.getOutputStream)) {
    LcpHandShake.loopOnPppPackets(tc.getInputStream, tc.getOutputStream)
    PapAuthentication.loopOnPppPackets(tc.getInputStream, tc.getOutputStream)
    IpcpHandShake.loopOnPppPackets(tc.getInputStream, tc.getOutputStream)
    println("My IP: " + IpcpHandShake.myIp)
    println("Peer IP: " + IpcpHandShake.peerIp)
  }
  else
    println("No carrier.")

  tc.disconnect
}
}

object LcpHandShake {
  def loopOnPppPackets(in_s : InputStream, out_s : OutputStream) : Unit = {
    var octet : Byte = 0
    var data = List[Byte]()

    var lastReceivedTime = System.currentTimeMillis
    var lastReqSentTime = System.currentTimeMillis

    while(System.currentTimeMillis - lastReceivedTime < Parameters.timeOutMillis) {
      if(in_s.available > 0) {
        lastReceivedTime = System.currentTimeMillis
        octet = in_s.read.toByte
        if(octet == 0x7e) {
          if(processLcp(data.reverse, out_s))
            return
        } else {
          data = List[Byte]()
          data = octet :: data
        }
      }

      if(System.currentTimeMillis - lastReqSentTime > Parameters.ourConfigReqInterval) {
        sendConfigReq(out_s)
        lastReqSentTime = System.currentTimeMillis
      }
    }

    throw new RuntimeException("LCP : Connection time out")
  }

  private def processLcp(data : List[Byte], out_s : OutputStream) : Boolean = {
    val packet = ParseUtils.decodeEscape(data)
    print("lcp(raw): ")
    ParseUtils.printlnHex(data)
    print("lcp(escaped): ")
    ParseUtils.printlnHex(packet)
    print("lcp(isvalid): ")
    println(isValidLcpPacket(packet))

    if(isValidLcpPacket(packet)) {
      if(packet(4) == 0x01.toByte) {
        print("lcp(ack): ")
        ParseUtils.printlnHex(makeConfigAck(packet))
        out_s.write(makeConfigAck(packet).toArray)
        out_s.flush
        return false
      } else if(packet(4) == 0x02.toByte) {
        return true
      }
    }
    return false
  }

  private def sendConfigReq(out_s : OutputStream) : Unit = {
    var req = Parameters.ourLcpConfigReqOptions
    val size = req.length + 4
    val sizeByteLow = (size & 0xFF).toByte
    val sizeByteHigh = ((size >> 8) & 0xFF).toByte
    req = sizeByteHigh :: sizeByteLow :: req
    req = 0x01.toByte :: Parameters.getNewIdCounter :: req
    req = List(0xFF, 0x03, 0xC0, 0x21).map(_.toByte) ::: req
    req = FCSCalculator.attachFCS16(req)
    req = ParseUtils.encodeEscape(req)
    req = 0x7e.toByte +: req :+ 0x7e.toByte
    print("lcp(sending): ")
    ParseUtils.printlnHex(req)
    out_s.write(req.toArray)
    out_s.flush
  }

  private def isValidLcpPacket(data : List[Byte]) : Boolean = {
    (data.length > 4) &&
    (data.slice(0,4) == List(0xFF, 0x03, 0xC0, 0x21).map(_.toByte)) &&
    FCSCalculator.isFCS16Good(data) &&
    isLcpLengthOk(data)
  }

  private def isLcpLengthOk(data : List[Byte]) : Boolean = {
    val nominalLength = (data(6) << 8) + data(7)
    data.length == (nominalLength + 6)
  }

  private def makeConfigAck(req : List[Byte]) : List[Byte] = {
    var ack = req.drop(5).dropRight(2)
    ack = 0x02.toByte :: ack
    ack = List(0xFF, 0x03, 0xC0, 0x21).map(_.toByte) ::: ack
    ack = FCSCalculator.attachFCS16(ack)
    ack = ParseUtils.encodeEscape(ack)
    ack = 0x7e.toByte +: ack :+ 0x7e.toByte
    ack
  }
} // object LcpHandShake

object PapAuthentication {
  def loopOnPppPackets(in_s : InputStream, out_s : OutputStream) : Unit = {
    var octet : Byte = 0
    var data = List[Byte]()

    var lastReceivedTime = System.currentTimeMillis
    var lastReqSentTime = System.currentTimeMillis
    var counter = 0

    while((System.currentTimeMillis - lastReceivedTime < Parameters.timeOutMillis) && counter < 10) {
      if(in_s.available > 0) {
        lastReceivedTime = System.currentTimeMillis
        octet = in_s.read.toByte
        if(octet == 0x7e) {
          if(processPap(data.reverse))
            return
          data = List[Byte]()
        } else {
          data = octet :: data
        }
      }

      if(System.currentTimeMillis - lastReqSentTime > Parameters.ourConfigReqInterval) {
        sendAuthReq(out_s)
        lastReqSentTime = System.currentTimeMillis
        counter += 1
      }
    }
    throw new RuntimeException("Connection time out")
  }

  private def processPap(data : List[Byte]) : Boolean = {
    print("pap(raw): ")
    printlnHex(data)
    val packet = decodeEscape(data)
    print("pap(escaped): ")
    printlnHex(packet)

    if(isValidPapPacket(packet)) {
      println("pap(is valid)")
      if(packet(4) == 0x02.toByte)
        return true
    }

    return false
  }

  private def isValidPapPacket(data : List[Byte]) : Boolean = {
    (data.length > 4) &&
    (data.slice(0,4) == List(0xFF, 0x03, 0xC0, 0x23).map(_.toByte)) &&
    FCSCalculator.isFCS16Good(data) &&
    isPapLengthOk(data)
  }

  private def isPapLengthOk(data : List[Byte]) : Boolean = {
    val nominalLength = (data(6) << 8) + data(7)
    data.length == (nominalLength + 6)
  }

  private def sendAuthReq(out_s : OutputStream) : Unit = {
    var req = Parameters.ourAuthReq
    val size = req.length + 4
    val sizeByteLow = (size & 0xFF).toByte
    val sizeByteHigh = ((size >> 8) & 0xFF).toByte
    req = sizeByteHigh :: sizeByteLow :: req
    req = 0x01.toByte :: Parameters.getNewIdCounter :: req
    req = List(0xFF, 0x03, 0xC0, 0x23).map(_.toByte) ::: req
    req = FCSCalculator.attachFCS16(req)
    req = encodeEscape(req)
    req = 0x7e.toByte +: req :+ 0x7e.toByte
    print("pap(sending): ")
    printlnHex(decodeEscape(req))
    out_s.write(req.toArray)
    out_s.flush
  }
} // object PapAuthentication

object IpcpHandShake {
  private var ourIpcpReqOptions = Parameters.ourIpcpInitialReqOptions
  private var peerIpValue = List[Byte]()
  private var myIpValue = List[Byte]()

  def peerIp = peerIpValue
  def myIp = myIpValue

  def loopOnPppPackets(in_s : InputStream, out_s : OutputStream) : List[Byte] = {
    var octet : Byte = 0
    var data = List[Byte]()

    var lastReceivedTime = System.currentTimeMillis
    var lastReqSentTime = System.currentTimeMillis
    var counter = 0

    while((System.currentTimeMillis - lastReceivedTime < Parameters.timeOutMillis) && counter < 10) {
      if(in_s.available > 0) {
        lastReceivedTime = System.currentTimeMillis
        octet = in_s.read.toByte
        if(octet == 0x7e) {
          processIPCP(data.reverse, out_s) match {
            case None => { }
            case Some(l) => {
              myIpValue = l
              return l
            }
          }
          data = List[Byte]()
        } else {
          data = octet :: data
        }
      }

      if(System.currentTimeMillis - lastReqSentTime > Parameters.ourConfigReqInterval) {
        sendIpcpConfigReq(out_s)
        lastReqSentTime = System.currentTimeMillis
        counter += 1
      }
    }
    throw new RuntimeException("Connection time out")
  }

  private def processIPCP(data : List[Byte], out_s : OutputStream) : Option[List[Byte]] = {
    print("ipcp(raw): ")
    printlnHex(data)
    val packet = decodeEscape(data)
    print("ipcp(escaped): ")
    printlnHex(packet)

    if(isValidIpcpPacket(packet)) {
      packet(4).toInt match {
        case 0x01 => {
          peerIpValue = packet.slice(10,14)
          val ack = makeIpcpConfigAck(packet)
          print("ipcp(sending): ")
          printlnHex(decodeEscape(ack))
          out_s.write(ack.toArray)
          out_s.flush
          None
        }
        case 0x02 =>
          Some(packet.slice(10,14))
        case 0x03 => {
          ourIpcpReqOptions = 0x03.toByte :: 0x06.toByte :: packet.slice(10,14)
          None
        }
      }
    }
    else
      None
  }

  private def isValidIpcpPacket(data : List[Byte]) : Boolean = {
    (data.length > 4) &&
    (data.slice(0,4) == List(0xFF, 0x03, 0x80, 0x21).map(_.toByte)) &&
    FCSCalculator.isFCS16Good(data) &&
    isIpcpLengthOk(data)
  }

  private def isIpcpLengthOk(data : List[Byte]) : Boolean = {
    val nominalLength = (data(6) << 8) + data(7)
    data.length == (nominalLength + 6)
  }

  private def sendIpcpConfigReq(out_s : OutputStream) : Unit = {
    var req = ourIpcpReqOptions
    val size = req.length + 4
    val sizeByteLow = (size & 0xFF).toByte
    val sizeByteHigh = ((size >> 8) & 0xFF).toByte
    req = sizeByteHigh :: sizeByteLow :: req
    req = 0x01.toByte :: Parameters.getNewIdCounter :: req
    req = List(0xFF, 0x03, 0x80, 0x21).map(_.toByte) ::: req
    req = FCSCalculator.attachFCS16(req)
    req = encodeEscape(req)
    req = 0x7e.toByte +: req :+ 0x7e.toByte
    print("ipcp(sending): ")
    printlnHex(decodeEscape(req))
    out_s.write(req.toArray)
    out_s.flush
  }

  private def makeIpcpConfigAck(req : List[Byte]) : List[Byte] = {
    var ack = req.drop(5).dropRight(2)
    ack = 0x02.toByte :: ack
    ack = List(0xFF, 0x03, 0x80, 0x21).map(_.toByte) ::: ack
    ack = FCSCalculator.attachFCS16(ack)
    ack = encodeEscape(ack)
    ack = 0x7e.toByte +: ack :+ 0x7e.toByte
    ack
  }
} // object IpcpHandShake