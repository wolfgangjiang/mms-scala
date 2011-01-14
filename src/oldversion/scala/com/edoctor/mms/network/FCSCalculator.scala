package com.edoctor.mms.network

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 1:01:49 PM
 * To change this template use File | Settings | File Templates.
 */

object FCSCalculator {
  private val fcsTable16 = calculateFCSTable16

  private def calculateFCSTable16():List[Int] = {
    var v = 0
    var theTable = List[Int]()

    for(b <- 0 until 256) {
      v = b
      for(i <- 1 to 8)
        v =
          if((v & 1) != 0)
            ((v >> 1) ^ 0x8408)
          else
            v >> 1
      theTable = (v & 0xFFFF) :: theTable
    }
    theTable.reverse
  }

  def getFCS16(data : List[Byte]) : Int = {
    var fcs = 0xFFFF

    for(octet <- data)
      fcs = ((fcs >> 8) ^ fcsTable16((fcs ^ octet) & 0xff)) & 0xffff

    (~fcs) & 0xffff
  }

  def isFCS16Good(data : List[Byte]) : Boolean = {
    var fcs = 0xFFFF

    for(octet <- data)
      fcs = ((fcs >> 8) ^ fcsTable16((fcs ^ octet) & 0xff)) & 0xffff

    fcs == 0xf0b8
  }

  def attachFCS16(data : List[Byte]) : List[Byte] = {
    val fcs = getFCS16(data)
    val fcsLowOctet = (fcs & 0xFF).toByte
    val fcsHighOctet = ((fcs >> 8) & 0xFF).toByte
    data ++ List(fcsLowOctet, fcsHighOctet)
  }
}