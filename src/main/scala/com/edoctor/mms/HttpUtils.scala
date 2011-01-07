package com.edoctor.mms

import org.apache.commons.net.telnet.TelnetClient
import net.tambur.mms.{MMConstants, MMMessage}
import java.io._

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 1:06:28 PM
 * To change this template use File | Settings | File Templates.
 */

object HttpUtils {
  // =================================================================
// 以下是http部分
// =================================================================


def makeMmsMessage : MMMessage = {
  val msg = new MMMessage()
  msg.setMessageType(MMConstants.MESSAGE_TYPE_M_SEND_REQ)
  msg.setTransactionId("0001")
  msg.setFrom("+8613818276284/TYPE=PLMN")
  msg.setTo("+8613501800943/TYPE=PLMN")
  msg.setSubject("mms test")
  msg.setVersion(1)
  msg.setContentType("application/vnd.wap.multipart.mixed")


  val fImage = new File("./2.gif")
  val data = new Array[Byte](fImage.length.toInt)
  val dis = new DataInputStream(new FileInputStream(fImage))
  dis.readFully(data)
  dis.close()

  msg.addPart("image/gif", data, false, null, null)
  msg
}

def makeMmsHttpHeader(msg : MMMessage) : String = {
//  "POST / HTTP/1.1\r\n" +
  "POST http://mmsc.monternet.com/ HTTP/1.1\r\n" +
//  "GET http://3g.sina.com.cn/?t=29293 HTTP/1.1\r\n" +
  "Accept: */*\r\n" +
  "Content-Length: " + msg.encode.length.toString + "\r\n" +
  "Content-type: application/vnd.wap.mms-message\r\n" +
  "User-Agent: SAMSUNG-SGH-E908/NetFront 3.2/WAP2.0 Profile/MIDP-2.0 Configuration/CLDC-1.1\r\n" +
//  "Accept: application/vnd.wap.mms-message, image/vnd.wap.wbmp, image/png, image/jpeg, image/gif, text/x-iMelody, text/x-imelody, application/x-midi, audio/midi, audio/mid, audio/x-mid, image/bmp, audio/mp3, audio/x-midi, audio/amr, application/vnd.smaf, application/vnd.wap.mms-message x-wap-profile:http://wap.samsungmobile.com/uaprof/e908_10.xml\r\n" +
   //  "Content-type: application/vnd.wap.multipart.mixed\r\n" +
  "Host: 10.0.0.172\r\n" +
//  "X-Online-Host: mmsc.monternet.com\r\n" +
  "\r\n\r\n"
}

def testMms(in: InputStream, out: OutputStream): Unit = {
  val msg = makeMmsMessage



  if(PPPUtils.beforeFirstFlag(in, out)) {
    LcpHandShake.loopOnPppPackets(in, out)
    PapAuthentication.loopOnPppPackets(in, out)
    IpcpHandShake.loopOnPppPackets(in, out)
    println("My IP: " + ParseUtils.ipToString(IpcpHandShake.myIp))
    println("Peer IP: " + ParseUtils.ipToString(IpcpHandShake.peerIp))
    val sock = new SocketOverPpp(
      "10.0.0.172", 80,
//      ipToString(IpcpHandShake.peerIp), 80,
      ParseUtils.ipToString(IpcpHandShake.myIp), 2048,
      in, out
    )
    sock.start
    // TicManagerActor ! ('register, sock)
    (new Tic(sock)).start

    sock.write(makeMmsHttpHeader(msg))
    sock.write(msg.encode.toList)
    var buf = List[Byte]()
    while(true) {
      if(sock.available > 0) {
        val octet = sock.read.toByte
        // System.err.print(octet.toChar)
        if(octet == 10) { // '\n'
          System.err.println(new String(buf.reverse.toArray, "UTF-8"))
          buf = List[Byte]()
        }
        else buf = octet :: buf
      }
    }
  }
  else
    println("No carrier.")

  TicManagerActor ! 'stop
}
}