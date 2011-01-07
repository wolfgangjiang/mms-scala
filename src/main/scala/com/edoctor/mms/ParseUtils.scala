package com.edoctor.mms

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 12:57:54 PM
 * To change this template use File | Settings | File Templates.
 */

object ParseUtils {
  def listToHex(list: List[Byte]): String =
    list.map(x => String.format("%02X", new java.lang.Byte(x))).mkString(" ")


  def ipToString(ipList: List[Byte]): String = {
    assert(ipList.length == 4)
    ipList.map(x => (x.toInt & 0xFF)).mkString(".")
  }


  def printlnHex(data: List[Byte]): Unit =
    println(listToHex(data))


  def parseIp(ipStr: String): List[Byte] = {
    val ipList = ipStr.split("\\.").toList.map(s => (s.toInt & 0xFF).toByte)
    assert(ipList.length == 4)
    ipList
  }

  def decodeEscape(list: List[Byte]): List[Byte] = {
    if (list.isEmpty)
      List[Byte]()
    else if (list.head == 0x7d)
      (list.tail.head ^ 0x20).toByte :: decodeEscape(list.tail.tail)
    else
      list.head :: decodeEscape(list.tail)
  }


  def encodeEscape(list: List[Byte]): List[Byte] = {
    def needsEscape(data: Byte): Boolean =
      (data < 0x20) || (List(0x7e, 0x7d, 0x91, 0x93) contains data)

    if (list.isEmpty)
      List[Byte]()
    else if (needsEscape(list.head))
      0x7d.toByte :: (list.head ^ 0x20).toByte :: encodeEscape(list.tail)
    else
      list.head :: encodeEscape(list.tail)
  }
}