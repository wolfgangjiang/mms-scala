package com.edoctor.mms.network

import net.lag.logging.Logger

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 5:02:57 PM
 * To change this template use File | Settings | File Templates.
 */

case class PppPacket(protocol_id_high: Byte, protocol_id_low: Byte, data: List[Byte]) extends PacketTrait{
  //calculate fcs and encode
  def to_raw_bytes = {
    val data_with_fcs = FCSCalculator.attachFCS16(PppPacket.ADDRESS :: PppPacket.CONTROL :: protocol_id_high :: protocol_id_low :: data)
    PppPacket.FLAG ::encode_escape(data_with_fcs) ++ List(PppPacket.FLAG)
  }

  def to_print_string = list_to_hex(PppPacket.ADDRESS :: PppPacket.CONTROL :: protocol_id_high :: protocol_id_low :: data)
  
  private def encode_escape(list: List[Byte]): List[Byte] = {
    def needsEscape(data: Byte): Boolean = 
    (data < 0x20 && data >= 0) || (List(PppPacket.FLAG, 0x7d, 0x91, 0x93) contains data)

    if (list.isEmpty)
      List[Byte]()
    else if (needsEscape(list.head))
      0x7d.toByte :: (list.head ^ 0x20).toByte :: encode_escape(list.tail)
    else
      list.head :: encode_escape(list.tail)
  }

}
case object InvalidPppPacket extends PppPacket(0x00, 0x00, List.empty[Byte])
object PppPacket extends PacketTrait {
  val log = Logger.get
  val FLAG: Byte = 0x7e
  val ADDRESS: Byte = 0xff.toByte
  val CONTROL: Byte = 0x03
  
  def create(raw_bytes: List[Byte]) = {
    decode_escape(raw_bytes) match {
      case FLAG :: ADDRESS :: CONTROL :: protocol_id_high :: protocol_id_low :: rest =>
        log.trace("A valid ppp packet created.")
        PppPacket(protocol_id_high, protocol_id_low, rest.dropRight(2)) //drop fcs
      case _ =>
        log.warning("An invalid ppp packet created:", list_to_hex(raw_bytes))
        InvalidPppPacket
    }
  }


  private def decode_escape(list: List[Byte]): List[Byte] = {
    if (list.isEmpty)
      List[Byte]()
    else if (list.head == 0x7d)
      (list.tail.head ^ 0x20).toByte :: decode_escape(list.tail.tail)
    else
      list.head :: decode_escape(list.tail)
  }

}