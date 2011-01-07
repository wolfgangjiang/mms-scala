package com.edoctor.mms

import java.io.{InputStream, OutputStream}
import network.FCSCalculator
import scala.actors.Actor
import ParseUtils._
import TCPUtils._

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 12:51:13 PM
 * To change this template use File | Settings | File Templates.
 */

class SocketOverPpp(peerIpAddr : List[Byte],
                    peerPort : Int,
                    myIpAddr : List[Byte],
                    myPort : Int,
                    in_s : InputStream,
                    out_s : OutputStream) extends Actor {

  require(peerIpAddr.length == 4)
  require(myIpAddr.length == 4)

  val defaultTTL = 0xFF.toByte // default time to live of outbound ip datagram
  var IpIdCount = (math.random * 40000).toInt

  val myMSS = 400  // maximal segment size
  var sndUna = (math.random * 1e8).toInt // send & unack, pointer
  var sndNxt = sndUna // send next, pointer
  var sndWnd = 0 // peer receive window size, dummy init value
  val rcvWnd = 1024 // fixed for now, we do not expect much throughput
  var rcvNxt = 0 // dummy init value

  var rcvNxtChanged = false
  var bufferOfRead = List[Byte]()
  var bufferOfWrite = List[Byte]()
  var bufferOfRawReceive = List[Byte]()
  var unaSegments = List(List[Byte]()) // un-acknowledged segments
  val intervalOfSend = 50
  var counterOfSend = 0
  val intervalOfResend = 1000
  var counterOfResend = 0

  open()  // action of default constructor

  def this(peerIpStr : String, peerPort : Int,
           myIpStr : String, myPort : Int,
           in_s : InputStream, out_s : OutputStream) = {
    this(ParseUtils.parseIp(peerIpStr), peerPort,
         ParseUtils.parseIp(myIpStr), myPort, in_s, out_s)
  }

  def open() : Boolean = {
    var octet : Byte = 0
    var data = List[Byte]()

    var lastReceivedTime = System.currentTimeMillis
    var counter = 0

    sendFirstConnectionSyn

    while((System.currentTimeMillis - lastReceivedTime < Parameters.timeOutMillis) && counter < 10) {
      if(in_s.available > 0) {
        lastReceivedTime = System.currentTimeMillis
        octet = in_s.read.toByte
        if(octet == 0x7e) {
          if(processConnectionEstablishment(data.reverse))
            return true // successfully connected
          data = List[Byte]()
        } else {
          data = octet :: data
        }
      }
    }
    false
  }

  def processConnectionEstablishment(data : List[Byte]) : Boolean = {
    val frame = ParseUtils.decodeEscape(data)
    print("tcp/ip(escaped): ")
    printlnHex(frame)

    // won't send if received segment is not a valid SYN+ACK
    val result = sendTcpSegConnectionAck(decodeEscape(frame))

    println("tcp/ip sndUna = " + sndUna + " " + listToHex(splitDoubleWord(sndUna)))
    println("tcp/ip sndNxt = " + sndNxt + " " + listToHex(splitDoubleWord(sndNxt)))
    println("tcp/ip sndWnd = " + sndWnd + " " + listToHex(splitDoubleWord(sndWnd)))
    println("tcp/ip rcvWnd = " + rcvWnd + " " + listToHex(splitDoubleWord(rcvWnd)))
    println("tcp/ip rcvNxt = " + rcvNxt + " " + listToHex(splitDoubleWord(rcvNxt)))

    result // 返回值
  }

  // 这个syn segment用于发起连接
  def makeTcpSegSyn : List[Byte] = {
    var seg = List[Byte]()
    seg = splitWord(0x0204) ::: splitWord(myMSS) ::: seg // option of MSS
    seg = splitWord(0x0000) ::: seg // urgent pointer unused
    seg = splitWord(0x0000) ::: seg // dummy checksum，稍后会填充进去。
    seg = splitWord(rcvWnd) ::: seg // receive window size
    seg = 0x02.toByte :: seg // control bits = SYN
    seg = (((seg.length + 13) >> 2) << 4).toByte :: seg // Data Offset
    // 上述data offset，在当时的seg左侧还要添加13字节，然后除以4（右移2位），
    // 然后放到字节的高4位中去（左移4位）。
    seg = splitDoubleWord(0) ::: seg // acknowledge number unused
    seg = splitDoubleWord(sndUna - 1) ::: seg // my sequence num
    seg = splitWord(peerPort) ::: seg // destination server port
    seg = splitWord(myPort) ::: seg // source port, also my ephemeral port
    seg = fillInTcpChecksum(seg)
    seg
  }

  def computeOutboundTcpChecksum(seg : List[Byte]) : Int = {
    var pseudoHeader = splitWord(seg.length)
    pseudoHeader = splitWord(0x0006) ::: pseudoHeader
    pseudoHeader = myIpAddr ::: peerIpAddr ::: pseudoHeader
    computeChecksum(pseudoHeader ::: seg)
  }

  def isInboundTcpChecksumGood(seg : List[Byte]) : Boolean = {
    var pseudoHeader = splitWord(seg.length)
    pseudoHeader = splitWord(0x0006) ::: pseudoHeader
    pseudoHeader = peerIpAddr ::: myIpAddr ::: pseudoHeader
    0 == computeChecksum(pseudoHeader ::: seg)
  }

  // 为要发送的tcp包填充校验和，返回填充后的tcp包
  def fillInTcpChecksum(seg : List[Byte]) : List[Byte] = {
    val checksum = computeOutboundTcpChecksum(seg)
    seg.take(16) ::: splitWord(checksum) ::: seg.drop(18)
  }

  def makeOutboundIpDatagram(tcpSeg : List[Byte]) : List[Byte] = {
    var header = List[Byte]() // We don't have IP options
    header = myIpAddr ::: peerIpAddr ::: header
    header = splitWord(0) ::: header // dummy checksum to be filled in later
    header = 0x06.toByte :: header // protocol = TCP = 0x06
    header = defaultTTL :: header // time to live
    header = splitWord(0) ::: header // We won't make fragments
    header = splitWord(IpIdCount) ::: header // Identifier of datagram
    IpIdCount += 1
    header = splitWord(4 + header.length + tcpSeg.length) ::: header // total length
    header = 0x00.toByte :: header // "Type of Service" unused
    // version and header length:
    header = (0x40 | (header.length + 1 >> 2)).toByte :: header
    header = fillInIpHeaderChecksum(header)
    header ::: tcpSeg
  }

  // 为要发送的IP包header填充校验和，返回填充后的IP header
  def fillInIpHeaderChecksum(header : List[Byte]) : List[Byte] = {
    val checksum = computeChecksum(header)
    header.take(10) ::: splitWord(checksum) ::: header.drop(12)
  }

  def makeOutboundPppFrame(ipDatagram : List[Byte]) : List[Byte] = {
    var frame = ipDatagram
    frame = splitWord(0x0021) ::: frame // protocol 0x0021 for IPv4
    frame = splitWord(0xFF03) ::: frame // fixed address and control bytes
    frame = FCSCalculator.attachFCS16(frame)
    frame = encodeEscape(frame)
    frame = 0x7e.toByte +: frame :+ 0x7e.toByte
    frame
  }

  def sendFirstConnectionSyn : Unit = {
    val segment = makeTcpSegSyn
    val datagram = makeOutboundIpDatagram(segment)
    val frame = makeOutboundPppFrame(datagram)

    print("tcp/ip(syn): ")
    printlnHex(decodeEscape(frame))
    out_s.write(frame.toArray)
    out_s.flush
  }

  // return true if incomingFrame is a valid ACK+SYN, return false otherwise
  def sendTcpSegConnectionAck(incomingFrame : List[Byte]) : Boolean = {
    if(isValidConnectionSynAck(incomingFrame)) {
      val incomingDatagram = incomingFrame.drop(4).dropRight(2)
      val incomingSegment =
        incomingDatagram.drop((incomingDatagram.head & 0x0F) * 4)
      rcvNxt = byteListToInt(incomingSegment.slice(4, 8)) + 1
      sndWnd = byteListToInt(incomingSegment.slice(14, 16))
      val segment = makeTcpSegConnectionAck(incomingSegment)
      val datagram = makeOutboundIpDatagram(segment)
      val frame = makeOutboundPppFrame(datagram)

      print("tcp/ip(ack): ")
      printlnHex(decodeEscape(frame))
      out_s.write(frame.toArray)
      out_s.flush

      true
    }
    else
      false
  }

  def makeTcpSegConnectionAck(incomingData : List[Byte]) : List[Byte] = {
    var seg = List[Byte]()
    seg = splitWord(0x0000) ::: seg // urgent pointer unused
    seg = splitWord(0x0000) ::: seg // dummy checksum to be filled later
    seg = splitWord(rcvWnd) ::: seg
    seg = 0x10.toByte :: seg // control bits = ACK
    seg = (((seg.length + 13) >> 2) << 4).toByte :: seg // Data Offset
    // 上述data offset，在当时的seg左侧还要添加13字节，然后除以
    // 4（右移2位），然后放到字节的高4位中去（左移4位）。
    seg = splitDoubleWord(rcvNxt) ::: seg // acknowledge num
    seg = splitDoubleWord(sndNxt) ::: seg // sequence num
    seg = splitWord(peerPort) ::: seg // destination server port
    seg = splitWord(myPort) ::: seg // source port, my ephemeral port
    seg = fillInTcpChecksum(seg)
    seg
  }

  def isValidConnectionSynAck(incomingFrame : List[Byte]) : Boolean = {
    if(!isValidPppFrameOverIp(incomingFrame))
      return false
    val incomingDatagram = incomingFrame.drop(4).dropRight(2)
    if(!isValidIpDatagram(incomingDatagram))
      return false
    val incomingSegment =
      incomingDatagram.drop((incomingDatagram.head & 0x0F) * 4)
    if(!isValidTcpSegment(incomingSegment))
      return false
    if(incomingSegment(13) != 0x12) // control bit of SYN & ACK
      return false
    if(byteListToInt(incomingSegment.slice(8,12)) != sndUna)
      return false
    return true
  }

  def isValidPppFrameOverIp(frame : List[Byte]) : Boolean = {
    (frame.length > 4) &&
    (frame.slice(0,4) == splitDoubleWord(0xFF030021)) &&
    FCSCalculator.isFCS16Good(frame)
  }

  // 假定是收到的datagram，所以source ip地址应该是peer ip，而
  // destination ip地址应该是my ip
  def isValidIpDatagram(datagram : List[Byte]) : Boolean = {
    val headerLength = (datagram.head & 0x0F) * 4
    if(headerLength > datagram.length) return false
    val header = datagram.take(headerLength)
    val totalLength = byteListToInt(header.slice(2,4))
    if(totalLength != datagram.length) return false
    if(header(9) != 6) return false // protocol = 6 = TCP
    if(computeChecksum(header) != 0) return false
    if(header.slice(12,16) != peerIpAddr) return false // source IP addr
    if(header.slice(16,20) != myIpAddr) return false // destination IP addr

    return true
  }

  // 假定是收到的segment，所以source port应该是peer port，而
  // destination port应该是my port
  def isValidTcpSegment(segment : List[Byte]) : Boolean = {
    // source port
    if(byteListToInt(segment.slice(0,2)) != peerPort) return false
    // destination port
    if(byteListToInt(segment.slice(2,4)) != myPort) return false
    // checksum
    if(!isInboundTcpChecksumGood(segment)) return false

    return true
  }

  // actor的核心函数，用来处理一般的TCP收发，将数据保存在缓冲区内以供外
  // 部调用者读写。
  override def act() : Unit = {
    var done = false
    while(!done) {
      receive {
        case 'tic => { // 处理对远程的收发
          getFromRemote
          if(counterOfSend == intervalOfSend) {
            counterOfSend = 0
            putToRemote
          }
          else counterOfSend += 1
        }
        case 'read => {
          if(bufferOfRead.isEmpty)
            reply(0)
          else {
            val result = bufferOfRead.head.toInt
            bufferOfRead = bufferOfRead.tail
            reply(result)
          }
        }
        case ('write, data : List[Byte]) =>
          bufferOfWrite = bufferOfWrite ::: data
        case 'available =>
          reply(bufferOfRead.length)
        case 'stop =>
          done = true
      }
    }
  }

  def getFromRemote : Unit = {
    var octet : Byte = 0
    while(in_s.available > 0) {
      octet = in_s.read.toByte
      if(octet == 0x7e) {
        processNormalPppFrame(bufferOfRawReceive.reverse)
        bufferOfRawReceive = List[Byte]()
      } else
        bufferOfRawReceive = octet :: bufferOfRawReceive
    }
  }

  def processNormalPppFrame(data : List[Byte]) : Unit = {
    val frame = decodeEscape(data)
    print("tcp/ip(escaped): ")
    printlnHex(frame)
    println(frame.map(_.toChar).mkString)

    if(isValidNormalTcpOverPpp(frame)) {
      val datagram = frame.drop(4).dropRight(2)
      val segment = datagram.drop((datagram.head & 0x0F) * 4)

      if((segment(13) & 0x01) != 0)  {// control bit of FIN
        //println("control bit FIN detected but ignored.")
        println("control bit FIN detected")
        System.exit(0)
      }
      if((segment(13) & 0x04) != 0)  {// control bit of RST
        //println("control bit RST detected but ignored.")
        println("control bit RST detected.")
        throw new java.lang.RuntimeException("RST detected.")
        System.exit(1)
      }

      val payload = segment.drop((segment(12) >> 4) * 4)
      println("tcp header length: " + ((segment(12) >> 4) * 4))

      bufferOfRead = bufferOfRead ::: payload
      rcvNxt = byteListToInt(segment.slice(4,8))
      sndUna = byteListToInt(segment.slice(8,12))
      sndWnd = byteListToInt(segment.slice(14,16))
      rcvNxtChanged = true
    }
  }

  def isValidNormalTcpOverPpp(frame : List[Byte]) : Boolean = {
    return (frame.length > 20) // very simplified check

    if(!isValidPppFrameOverIp(frame))
      return false

    println("is valid frame")

    val datagram = frame.drop(4).dropRight(2)
    if(!isValidIpDatagram(datagram))
      return false

    println("is valid datagram")

    val segment = datagram.drop((datagram.head & 0x0F) * 4)
    if(!isValidTcpSegment(segment))
      return false

    println("is valid segment")

    val payloadLength =
      segment.length - ((segment(12) >> 4) * 4)
    if(payloadLength != (byteListToInt(segment.slice(4,8)) - rcvNxt))
      return false

    return true
  }

  def putToRemote : Unit = {
    unaSegments = unaSegments.filter{ x =>
      byteListToInt(x.slice(4,8)) > sndUna  }

    if(counterOfResend == intervalOfResend) {
      counterOfResend = 0

      for(s <- unaSegments)
        putSegmentToRemote(s)
    }
    else counterOfResend += 1


    if(bufferOfWrite.isEmpty && !rcvNxtChanged)
      return

    rcvNxtChanged = false
    var payload = bufferOfWrite
    if(payload.length > myMSS)
      payload = payload.take(myMSS)
    if(payload.length > sndWnd)
      return // to avoid Silly Window Syndrome

    val segment = makeTcpSegNormal(payload)
    putSegmentToRemote(segment)

    unaSegments = segment :: unaSegments
    bufferOfWrite = bufferOfWrite.drop(payload.length)
  }

  def makeTcpSegNormal(payload : List[Byte]) : List[Byte] = {
    var seg = payload
    seg = splitWord(0x0000) ::: seg // urgent pointer unused
    seg = splitWord(0x0000) ::: seg // dummy checksum to be filled in later
    seg = splitWord(rcvWnd) ::: seg // receive window size
    seg = 0x10.toByte :: seg // control bits = Ack
    seg = (((seg.length + 13 - payload.length) >> 2) << 4).toByte :: seg
    // 上述是data offset，在当时的seg左侧还要添加13字节，然后除以4（右
    // 移2位），然后放到字节的高四位去（左移4位）。
    seg = splitDoubleWord(rcvNxt) ::: seg // acknowledge num
    seg = splitDoubleWord(sndNxt) ::: seg // sequence num
    sndNxt = sndNxt + payload.length
    seg = splitWord(peerPort) ::: seg // destination server port
    seg = splitWord(myPort) ::: seg // source port, my ephemeral port
    seg = fillInTcpChecksum(seg)
    seg
  }

  def putSegmentToRemote(segment : List[Byte]) : Unit = {
    val datagram = makeOutboundIpDatagram(segment)
    val frame = makeOutboundPppFrame(datagram)

    print("tcp/ip(send): ")
    printlnHex(decodeEscape(frame))
//    println(decodeEscape(frame).map(_.toChar).mkString)
    out_s.write(frame.toArray)
    out_s.flush
  }

  // 若无数据可读，返回0，这与java标准库的InputStream的阻塞的做法不同，
  // 要注意。
  def read() : Int = {
    (this !? 'read) match {
      case x : Int => x
      case _ => 0
    }
  }

  def write(data : List[Byte]) : Unit = {
    this ! ('write, data)
  }

  def write(str : String) : Unit = {
    this.write(str.toList.map(_.toByte))
  }

  def available() : Int = {
    // val v = (this !? 'available)
    (this !? 'available) match {
      case x : Int => x
      case _ => 0
    }
  }
} // class SocketOverPpp
