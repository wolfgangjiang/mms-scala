package com.edoctor.mms

import org.apache.commons.net.telnet.TelnetClient

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 2:17:12 PM
 * To change this template use File | Settings | File Templates.
 */

object AppTest {
  def testActors : Unit = {
  val a1 = new TestActor(1)
  val a2 = new TestActor(2)
  val a3 = new TestActor(3)

  a1.start
  a2.start
  a3.start
  TicActor.start
  TicManagerActor.start

  TicManagerActor ! ('register, a1)
  TicManagerActor ! ('register, a2)
  TicManagerActor ! ('register, a2)
  TicManagerActor ! ('register, a3)

  Thread.sleep(10*1000)

  TicManagerActor ! ('unregister, a1)
  TicManagerActor ! ('unregister, a2)

  Thread.sleep(5*1000)

  TicManagerActor ! ('unregister, a3)
  TicManagerActor ! ('unregister, a3)

  a1 ! 'exit
  a2 ! 'exit
  a3 ! 'exit
}

// testActors

def testTcpIpPackaging() : Unit = {
  val d1 = """        45 00 00 6c
  92 cc 00 00
  38 06 e4 04
  92 95 ba 14
  a9 7c 15 95 """

  val d2 = """ 45 00
  00 2C 00 23 00 00 1E 06 84 1d 0B 01 01 45 0B 01
  01 46 """

  val d3 = "0B 01 01 45 0B 01 01 46 00 06 00 18 " +
  "A6 22 00 2B  00 00 BB BE  00 00 00 00  60 02 05 AA " +
  "17 ee 00 00  02 04 05 AA"

  //printlnHex(parseHex(d1))
  //printf("%x\n", computeChecksum(parseHex(d1)))
  // val sock = new SocketOverPpp("10.0.0.172", 80,
  //                             "10.177.100.100", 2048)
  //printlnHex(sock.makeOutboundIpDatagram(sock.makeTcpSegSyn))

}

// testTcpIpPackaging

val httpRequest =
  "GET http://www.baidu.com/ HTTP/1.1\r\n" +
"Accept: */*\r\n" +
"Accept-Language: zh-cn\r\n" +
"Host: www.baidu.com\r\n" +
"Proxy-Connection: Keep-Alive\r\n\r\n\r\n"


def testSocketConnection(): Unit = {
  //TicActor.start
  //TicManagerActor.start

  val tc = new TelnetClient

  tc.connect(Parameters.mmscatIP, Parameters.mmscatPort)

  if(PPPUtils.beforeFirstFlag(tc.getInputStream, tc.getOutputStream)) {
    LcpHandShake.loopOnPppPackets(tc.getInputStream, tc.getOutputStream)
    PapAuthentication.loopOnPppPackets(tc.getInputStream, tc.getOutputStream)
    IpcpHandShake.loopOnPppPackets(tc.getInputStream, tc.getOutputStream)
    println("My IP: " + ParseUtils.ipToString(IpcpHandShake.myIp))
    println("Peer IP: " + ParseUtils.ipToString(IpcpHandShake.peerIp))
    val sock = new SocketOverPpp(
      IpcpHandShake.peerIp, 80,
      IpcpHandShake.myIp, 2048,
      tc.getInputStream, tc.getOutputStream
    )
    sock.start
    // TicManagerActor ! ('register, sock)
    (new Tic(sock)).start
    sock.write(httpRequest)
    while(true) {
      if(sock.available > 0)
        System.err.print(sock.read.toChar)
    }
  }
  else
    println("No carrier.")

  tc.disconnect
  TicManagerActor ! 'stop
}

// testSocketConnection
}