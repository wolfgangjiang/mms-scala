package com.edoctor.mms.actors

import java.io.InputStream
import actors.{TIMEOUT, Actor}
import net.lag.logging.Logger
import com.edoctor.mms.network.{InvalidPppPacket, PppPacket}

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 5:17:11 PM
 * To change this template use File | Settings | File Templates.
 */

class PppRecipient(in: InputStream, session: ModemSession) extends Actor {
  val log = Logger.get
  //at the very beginning, no package, no received bytes
  def act = {
    log.debug("ppp recipient start")
    run(List.empty[Byte])
  }

  def run(byte_list: List[Byte]): Unit = {
    loop {
      //receive all the bytes from the input stream and split to packages
      val (packets: List[PppPacket], tail_list: List[Byte]) = to_ppp_packet(List.empty[PppPacket], do_receive(byte_list))
      //send each package to session
      packets.foreach(session ! _)

      reactWithin(1000) {
        case Terminate => //nothing, the actor terminate
        case TIMEOUT => run(tail_list)
      }
    }
  }

  protected def do_receive(tail_list: List[Byte]): List[Byte] = {
    //receive the bytes from the input stream
    def receive(result: List[Byte]): List[Byte] = {
      if (in.available > 0) {
        receive(in.read.toByte :: result)
      } else
        result
    }
    //merge result
    tail_list ++ receive(List.empty[Byte]).reverse
  }

  protected def to_ppp_packet(packet_list: List[PppPacket], byte_list: List[Byte]) = {
    byte_list.indexOf(PppPacket.FLAG) match {     //look up the start flag
      case -1 =>
        log.trace("No ppp packet found!")
        (packet_list, byte_list) //not ppp packet found
      case offset =>
        val data = byte_list.drop(offset) //found a ppp packet start flag, drop useless data on the left
        data.tail.indexOf(PppPacket.FLAG)  match {    //look up the finish flag
          case -1 => (packet_list, byte_list) //ppp packet transport not finished, process it in the next turn
          case 0 => (packet_list, byte_list.drop(1)) //clear the single ppp flag 0x7e
          case finished_offset =>
            val ppp_packet = PppPacket.create(data.slice(0, finished_offset + 1)) //ignore the finish flag
            log.debug("PPP packet received:" + ppp_packet.to_print_string)
            val result_packets = ppp_packet :: packet_list
            val tail_list = byte_list.drop(finished_offset)
            (result_packets, tail_list)
        }

    }
  }
}