package com.edoctor.mms

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 1:05:24 PM
 * To change this template use File | Settings | File Templates.
 */

object TCPUtils {
  // ===========================================================
  // 以下是TCP/IP的部分。
  // ===========================================================

  def computeChecksum(data: List[Byte]): Int = {
    def getHeadNum(data: List[Byte]): Int =
      if (data.length == 1)
        (data.head & 0xFF) << 8
      else
        ((data.head & 0xFF) << 8) + (data.tail.head & 0xFF)

    def recur(number: Int, data: List[Byte]): Int =
      if (data.isEmpty)
        number
      else {
        val simpleSum = number + getHeadNum(data)
        recur((simpleSum >> 16) + (simpleSum & 0xFFFF), data.drop(2))
      }

    (~recur(getHeadNum(data), data.drop(2)) & 0xFFFF) // bits inversed
  }

  def parseHex(str: String): List[Byte] =
    str.split("\\s+").toList.filterNot(_ == "").map(x => Integer.parseInt(x, 16).toByte)


  def splitWord(word: Int): List[Byte] =
    List(((word >> 8) & 0xFF).toByte, (word & 0xFF).toByte)

  def splitDoubleWord(doubleWord: Int): List[Byte] =
    splitWord((doubleWord >> 16) & 0xFFFF) :::
            splitWord(doubleWord & 0xFFFF)

  def byteListToInt(list: List[Byte]): Int = {
    def recur(list: List[Byte], number: Int): Int =
      if (list.isEmpty)
        number
      else
        recur(list.tail, (number << 8) + (list.head.toInt & 0xFF))

    recur(list, 0)
  }
}