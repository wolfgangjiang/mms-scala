package com.edoctor.mms.network

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Jan 5, 2011
 * Time: 1:38:06 PM
 * To change this template use File | Settings | File Templates.
 */

trait PacketTrait {
  def list_to_hex(list: List[Byte]): String =
    list.map(x => String.format("%02X", new java.lang.Byte(x))).mkString(" ")
}