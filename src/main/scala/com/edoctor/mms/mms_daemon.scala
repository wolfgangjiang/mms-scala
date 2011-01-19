package com.edoctor.mms

import net.rubyeye.xmemcached.utils.AddrUtil
import net.rubyeye.xmemcached.{ XMemcachedClientBuilder, MemcachedClient }
import net.rubyeye.xmemcached.command.KestrelCommandFactory

import org.apache.commons.httpclient.{ HttpClient, HttpStatus }
import org.apache.commons.httpclient.methods.GetMethod

import net.tambur.mms.{ MMMessage, MMConstants }

// 若一个port连续不能成功发送，可以考虑降低它的capacity，或者将它
// disable半小时左右，让modem休息一下，也许接下去就行了。如果还是不行，
// 则可能是余额不足或被封号。


object KestrelHandler {
  val queue_addr = read_kestrel_config("mms.conf")
  private var i_am_fine = true

  def is_fine = i_am_fine

  private def read_kestrel_config(filename : String) : String = {
    Log.info("KestrelHandler", "Read kestrel config from " + filename)
    try {
      val source = io.Source.fromFile(filename)
      source.getLines.find(line => line.startsWith("kestrel =")).get.drop(9).trim.split("\\s+").toList.mkString(":")
    } catch {
      case e : Exception => {
        Log.error("KestrelHandler", e)
        Log.error("KestrelHandler", "Error reading from config file: " + filename)
        exit(-1)
      }
    }    
  }
  
  private var client : MemcachedClient = initialize_client

  private def initialize_client : MemcachedClient = 
    try {
      val the_builder = 
        new XMemcachedClientBuilder(AddrUtil.getAddresses(queue_addr))
      the_builder.setCommandFactory(new KestrelCommandFactory())
      val the_client = the_builder.build
      the_client.setPrimitiveAsString(true) // 字符串前不附加flag
      the_client.setOptimizeGet(false) // 因为kestrel不支持bulk get
      the_client
    } catch {
      case e : Exception => {
        i_am_fine = false
        Log.error("KestrelHandler", e)
        return null
      }
    }

  def get : String = {
    try {
      val value : String = client.get("mms")
      i_am_fine = true
      value
    } catch {
      case e : Exception => {
        i_am_fine = false 
        Log.error("KestrelHandler", e)
        client = initialize_client
        return null
      }
    }
  }

  def put(thing : String) : Unit = {
    try {
      client.set("mms", 0, thing)
      i_am_fine = true
    } catch {
      case e : Exception => {
        i_am_fine = false 
        Log.error("KestrelHandler", e)
        client = initialize_client
        return null
      }
    }
  }

  def shutdown : Unit = client.shutdown

  def rearrange_timed_messages : Unit = {
    // today_string的格式类似于当天的"19970530"。
    val today_string : String = 
      (new java.text.SimpleDateFormat("yyyyMMdd")).format(
        java.util.Calendar.getInstance.getTime) 
    
    Log.info("KestrelHandler", "Starting rearrange queue for specified-timed messages of today.")

    try {
      var going = true    
      // 首先将当前队列分成两个队列，当天的和不是当天的。
      while(going) {
        val request : String = client.get("mms") 
        if(request == null) {
          going = false
        } else {
          val type_example = Array("a")
          val fields : Array[String] = (new com.google.gson.Gson).fromJson(request, type_example.getClass)
          if(!fields.isEmpty && fields(0) == today_string)
            client.set("mmstoday", 0, request)
          else
            client.set("mmsother", 0, request)
        }
      }

      // 然后将它们先后放回到原先的队列中。
      going = true
      while(going) {
        val request : String = client.get("mmstoday")
        if(request == null) going = false
        else client.set("mms", 0, request)
      }

      going = true
      while(going) {
        val request : String = client.get("mmsother")
        if(request == null) going = false
        else client.set("mms", 0, request)
      }
        
      i_am_fine = true
      Log.info("KestrelHandler", "Finished rearranging queue for specified-timed messages of today, successfully.")
    } catch {
      case e : Exception => {
        i_am_fine = false 
        Log.error("KestrelHandler", e)
        client = initialize_client
        return null
      }
    }
  }
}

class MalformedMessageException extends RuntimeException

object MmsDaemon {
  private val port_config : List[(String, Int, Int, Boolean)] = 
    read_port_config("mms.conf")
  private var remind_rearrange : Boolean = false

  // 本函数由一个自动定时的actor调用，每隔固定时间调用一次。
  def remind_rearrange_timed_messages : Unit = { remind_rearrange = true }

/*
List(
    ("192.168.10.230", 964, 200, true),
    ("192.168.10.230", 966, 200, true))
 */

  val ports : List[Port] = 
    (port_config.map( p => new ModemPort(p._1, p._2, p._3, p._4) ) :+ 
     new SPPort("Cosms SP        ", 1440, false, CosmsSPInfo.send_mms_method))

  private def read_port_config(filename : String) : List[(String, Int, Int, Boolean)] = {
    def find_port(line : String) : Option[(String, Int, Int, Boolean)] =
      if(line.startsWith("port =")) {
        val data = line.drop(6).trim.split("\\s+")
        Some((data(0), data(1).toInt, data(2).toInt, (data(3) == "true")))
      } else if(line.startsWith("port=")) {
        val data = line.drop(5).trim.split("\\s+")
        Some((data(0), data(1).toInt, data(2).toInt, (data(3) == "true")))
      } else None

    try {
      val source = io.Source.fromFile(filename)
      source.getLines.map(find_port).toList.filterNot(_ == None).map(
        x => x.get)
    } catch {
      case e : Exception => {
        Log.error("MMS daemon", e)
        Log.error("MMS daemon", "Error reading from config file: " + filename)
        exit(-1)
      }
    }
  }

  private def make_mms_message(request : String) : List[Byte] = {
    try {
      val type_example = Array("a")
      val fields : Array[String] = (new com.google.gson.Gson).fromJson(request, type_example.getClass)
      val date = fields(0)
      val target = fields(1)
      val subject = fields(2)
      val text = fields(3)
      val pic_url = fields(4)
      val mid_url : String = if(fields.length > 5) fields(5) else ""

      Log.debug("make_mms_message", "Got from kestrel queue.")
      Log.debug("make_mms_message", "target is: " + target)
      Log.debug("make_mms_message", "text is: " + text)
      Log.debug("make_mms_message", "pic_url is: " + pic_url)
      Log.debug("make_mms_message", "mid_url is: " + mid_url)

      Log.debug("make_mms_message", "Trying to download pic at " + pic_url)
      val pic_data = download(pic_url)
      Log.debug("make_mms_message", "Pic downloaded. Now trying to download mid at " + mid_url)
      val mid_data = if(mid_url == "") Nil else download(mid_url)
      Log.debug("make_mms_message", "Downloading finished, ")

      val msg = new MMMessage()
      msg.setMessageType(MMConstants.MESSAGE_TYPE_M_SEND_REQ)
      msg.setTransactionId("0001")
      msg.setFrom("+8615000279445/TYPE=PLMN")
      msg.setTo("+86" + target + "/TYPE=PLMN")
      msg.setSubject(" " + subject)
      msg.setVersion(1)
      msg.setContentType("application/vnd.wap.multipart.mixed")

      if(!pic_data.isEmpty)
        msg.addPart("image/gif", pic_data.toArray, false, null, null)

      /*
       val test_s = new java.io.DataOutputStream(
       new java.io.FileOutputStream(new java.io.File("a.gif")))
       test_s.write(pic_data.toArray, 0, pic_data.length)
       test_s.close

val test_s2 = new java.io.DataOutputStream(
new java.io.FileOutputStream(new java.io.File("a.mid")))
test_s2.write(mid_data.toArray, 0, mid_data.length)
test_s2.close
*/
      msg.addPart("text/plain; charset=\"utf-8\"", text.getBytes, false, null, null)

      if(!mid_data.isEmpty)
        msg.addPart("audio/midi; autostart=true", mid_data.toArray, false, null, null)

      Log.debug("make_mms_message", "All parts downloaded and added to message, now start encoding it.")

      val result : List[Byte] = msg.encode.toList

      Log.debug("make_mms_message", "Finished encoding.")

      result
    } catch {
      case e : Exception => {
        Log.error("make_mms_message", e)
        throw new MalformedMessageException
      }
    }
  }

  private def download(url : String) : List[Byte] = {
    Log.debug("download", "downloading " + url)

    val httpclient = new HttpClient()
    val httpget = new GetMethod(url)

    httpclient.getParams.setParameter("http.socket.timeout", 2000)
    httpclient.getParams.setParameter("http.connection.timeout", 2000)
    httpclient.getParams.setParameter("http.connection-manager.timeout", 2000L)

    try {
      val status_code = httpclient.executeMethod(httpget)

      if(status_code != HttpStatus.SC_OK) 
        Nil
      else
        httpget.getResponseBody.toList
    } catch {
      case e : Exception => {
        Log.error("download", e)
        return Nil
      }
    } finally {
      httpget.releaseConnection
    }
  }

  private def write_conf : Unit = {
    import java.io._
    val filename = "mms.conf"
    val kestrel_info =
      ("# kestrel queue address and port,\n" +
       "# where address and port must be separated with blank, not colon.\n" +
       "kestrel = " + KestrelHandler.queue_addr.replace(':', ' ') + "\n\n")

    val info_string = kestrel_info +
      ("# Following is modem port configuration.\n" +
       "# Each line must start with string \"port =\",\n" +
       "# four data fields are MODEM_IP, MODEM_PORT, DAY_CAPACITY and IS_ENABLED,\n" +
       "# and they must be separated with blank, not comma.\n" +
       "# When system is running, this file will be automatically saved every 2 seconds.\n")

    try {
      val writer = new FileWriter(filename)
      writer.write(info_string + ports.map(_.to_conf_string).mkString("\n"))
      writer.close
    } catch {
      case e : IOException => {
        Log.error("write_conf", e)
        Log.error("write_conf", "Error when auto writing to conf file" + filename)
      }
    }
  }

  /**
   * 在这里可以对ports进行多步操作，因为除了本对象之外，不会有其它
   * 线程使得一个ready的port变成“不ready”的。
   */
  def get_port : Option[Port] = {
    ports.foreach(_.kill_dangling_session)
    val ready_ports = ports.filter(_.is_ready)
    if(ready_ports.isEmpty)
      None
    else
      Some(ready_ports(util.Random.nextInt(ready_ports.length)))
  }

  def get_request : Option[String] = {
    val request = KestrelHandler.get
    if(request == null)
      None
    else
      Some(request)
  }

  private def is_of_future(request : String) : Boolean = {
    // today_string的格式类似于当天的"19970530"。
    val today_string : String = 
      (new java.text.SimpleDateFormat("yyyyMMdd")).format(
        java.util.Calendar.getInstance.getTime) 
    
    try {
      val type_example = Array("a")
      val fields : Array[String] = (new com.google.gson.Gson).fromJson(request, type_example.getClass)
      ((!fields.isEmpty) && 
       (!fields(0).isEmpty) &&
       (fields(0)(0).isDigit) &&
       (fields(0) > today_string))
    } catch {
      case e : Exception => 
        return false
    }
  }

  def main_loop : Unit = {
    ports.foreach{p =>
      Log.info("MMS daemon", p.to_conf_string) }
    var going = true
    Log.info("MMS daemon", "Main loop started")
    while(going) {
      try {
        // port无论去获取多少次都没有关系，它总是在那里等着。但是
        // request就不能贸然去获取，因为这种读取是破坏性的，所以一定要
        // 在有port可用的情况下才去获取request。若两者都有，就可以发送
        // 了。
        val port = get_port
        if(port != None) {
          val request = get_request
          if(request != None) {
            if(is_of_future(request.get)) {
              KestrelHandler.put(request.get)
              Log.debug("main_loop", request.get + " will be sent in future.")
            }
            else {
              Log.info("main_loop", "Trying to send :" + request.get)
              val msg : List[Byte] = make_mms_message(request.get)
              port.get.send_mms(request.get, msg)
            }
          }
        }
        
        Thread.sleep(2000)
        write_conf
        if(remind_rearrange) {
          remind_rearrange = false
          KestrelHandler.rearrange_timed_messages
          // 在进行上述的队列调整时，对队列的其它操作基本上是停下来的。
          // 只有零星的并发可能是ModemPort将发送失败的彩信请求放回到队
          // 列里，kestrel队列应该可以完善应付这种小的并发请求。而且，
          // 无论如何，定时在未来的请求总也不会提前被发送。
        }
      } catch {
        case e : MalformedMessageException => {
          Log.error("MMS daemon", e)
        }
        case e : Exception => {
          Log.error("MMS daemon", e)
          Log.error("MMS daemon", "Above is an overall exception that is not caught in subroutines.")
        }
      }
    }
  }
}

trait Port {
  def success_rate_string : String
  def is_ready : Boolean
  def change_capacity : Unit
  def capacity : Int
  def interval_of_send : Long
  def get_status_string : String
  def toggle_enabled : Unit
  def is_enabled : Boolean
  def kill_dangling_session : Unit
  def send_mms(request : String, msg_data : List[Byte]) : Unit
  def to_conf_string : String
}

// 一个ModemPort对象的生存期贯穿从彩信模块开始运行到结束运行，与之相
// 对，ModemSession对象则是为每条彩信建立一个。当ModemPort对象刚刚完成
// 初始化的时候，它的capacity和enabled参数来自于构造函数，而my_session
// 成员变量为null。
class ModemPort(the_ip_addr : String, the_tcp_port : Int, the_capacity : Int, the_enabled : Boolean) extends Port {
  val ip_addr = the_ip_addr
  val tcp_port = the_tcp_port
  val capacity_list = List(10,50,100,200,480,720)

  def interval_of_send = (1440 / my_capacity) * 60 * 1000L

  private var my_last_sent_time : Long = 0L
  private var my_session : MmsSession = null
//  private var my_session : DummySession = null
  private var history = List[Boolean]()
  private var enabled = the_enabled
  private var my_capacity = 
    capacity_list.reverse.find(x => x <= the_capacity) match {
      case Some(x) => x
      case None => 50
    }

  private var last_request : String = null

//  def weight = my_weight
//  def set_weight(value : Int) { my_weight = value }

  private def push_to_history(value : Boolean) : Unit = {
    history = value :: history
    if(history.length > 10)
      history = history.take(10)
    if(!value)
      KestrelHandler.put(last_request)
  }

  private def cooled_down : Boolean = 
    (System.currentTimeMillis - my_last_sent_time > interval_of_send)

  def success_rate_string : String = 
    if(history.isEmpty) "No Record"
    else 
      (history.count(x => x).toString + "/" +
       history.length.toString + " == " +
       (history.count(x => x) * 100 / history.length).toString + "%") 

  def is_ready : Boolean = 
    (get_status_string == "Ready")

  def change_capacity : Unit = {
    val place = capacity_list.indexOf(my_capacity)
    val new_place = 
      if(place < 0) 0
      else if(place + 1 >= capacity_list.length) 0
      else place + 1
    my_capacity = capacity_list(new_place)
  }

  def capacity = my_capacity

  def get_status_string : String = {
    def cooling_down_string : String = {
      val time = (interval_of_send - (System.currentTimeMillis - my_last_sent_time))
        ("Cool Down(" +
         (time / 60 / 1000).toString + ":" +
         ((time / 1000) % 60).toString + ")"
       )
    }

    def busy_string : String = {
      val time = (System.currentTimeMillis - my_last_sent_time)
        ("Busy(" +
         (time / 60 / 1000).toString + ":" +
         ((time / 1000) % 60).toString + ")"
       )
    }

    if(!enabled)
      "Disabled"
    else if(my_session != null) 
      if(my_session.getState == Thread.State.TERMINATED) {
        push_to_history(my_session.is_successful)
        my_session = null
        if(cooled_down) "Ready" else cooling_down_string
      } else busy_string
    else
      if(cooled_down) "Ready" else cooling_down_string
  }
  
  // 将 busy的端口强行disable掉，将会放弃当前发送的彩信，但是，
  // disable无论怎样切换，都不会改变cool down的时间限制。
  def toggle_enabled : Unit = {
    if(my_session != null) {
      my_session.interrupt
      my_session = null
      push_to_history(false)
    }
    enabled = !enabled
  }

  def is_enabled : Boolean = enabled

  def kill_dangling_session : Unit = 
    if((my_session != null) && cooled_down && enabled) {
      my_session.interrupt 
      my_session = null 
      push_to_history(false)
    }

  def send_mms(request : String, msg_data : List[Byte]) : Unit = {
    if(is_ready) {
      Log.info(this.toString, "sending mms")
      my_session = new MmsSession(ip_addr, tcp_port, msg_data)
//    my_session = new DummySession(ip_addr, tcp_port, msg_data)
      my_session.start
      my_last_sent_time = System.currentTimeMillis
      last_request = request
    } else 
      Log.error(this.toString, "Bug, trying to send msg via a port that is not ready.")
  }

  override def toString : String = 
    (ip_addr + " " + tcp_port)    // 为了打印整齐，长度必须为19

  def to_conf_string : String =
    ("port = " + 
     ip_addr + "  " + 
     tcp_port + "     " + 
     my_capacity + "     " + 
     enabled)
}

class SPPort(the_name : String, the_capacity : Int, the_enabled: Boolean, send_mms_method : (String, List[Byte]) => Boolean) extends Port {
  val my_name = the_name
  val capacity_list = List(10,50,100,200,480,720,1440)

  def interval_of_send = (1440 / my_capacity) * 60 * 1000L

  private var my_last_sent_time : Long = 0L
  private var history = List[Boolean]()
  private var enabled = the_enabled
  private var my_capacity = 
    capacity_list.reverse.find(x => x <= the_capacity) match {
      case Some(x) => x
      case None => 50
    }

  private var last_request : String = null

//  def weight = my_weight
//  def set_weight(value : Int) { my_weight = value }

  private def push_to_history(value : Boolean) : Unit = {
    history = value :: history
    if(history.length > 10)
      history = history.take(10)
    if(!value)
      KestrelHandler.put(last_request)
  }

  private def cooled_down : Boolean = 
    (System.currentTimeMillis - my_last_sent_time > interval_of_send)

  def success_rate_string : String = 
    if(history.isEmpty) "No Record"
    else 
      (history.count(x => x).toString + "/" +
       history.length.toString + " == " +
       (history.count(x => x) * 100 / history.length).toString + "%") 

  def is_ready : Boolean = 
    (get_status_string == "Ready")

  def change_capacity : Unit = {
    val place = capacity_list.indexOf(my_capacity)
    val new_place = 
      if(place < 0) 0
      else if(place + 1 >= capacity_list.length) 0
      else place + 1
    my_capacity = capacity_list(new_place)
  }

  def capacity = my_capacity

  def get_status_string : String = {
    def cooling_down_string : String = {
      val time = (interval_of_send - (System.currentTimeMillis - my_last_sent_time))
        ("Cool Down(" +
         (time / 60 / 1000).toString + ":" +
         ((time / 1000) % 60).toString + ")"
       )
    }

    def busy_string : String = {
      val time = (System.currentTimeMillis - my_last_sent_time)
        ("Busy(" +
         (time / 60 / 1000).toString + ":" +
         ((time / 1000) % 60).toString + ")"
       )
    }

    if(!enabled)
      "Disabled"
    else
      if(cooled_down) "Ready" else cooling_down_string
  }
  
  // 将 busy的端口强行disable掉，将会放弃当前发送的彩信，但是，
  // disable无论怎样切换，都不会改变cool down的时间限制。
  def toggle_enabled : Unit = {
    enabled = !enabled
  }

  def is_enabled : Boolean = enabled

  def kill_dangling_session : Unit = { } // do nothing

  def send_mms(request : String, msg_data : List[Byte]) : Unit = {
    Log.info(this.toString, "sending mms")
    last_request = request
    my_last_sent_time = System.currentTimeMillis
    push_to_history(send_mms_method(request,msg_data))
    Log.info(this.toString, "finished sending")
  }

  override def toString : String = my_name

  def to_conf_string : String =
    ("SP   = " + 
     my_name + "     " + 
     my_capacity + "     " + 
     enabled)
}

// 测试用
class DummySession(ip_addr : String, 
                   tcp_port : Int, 
                   msg_data : List[Byte]) extends Thread {
  import scala.util.Random

  val my_name = "DummySession [" + ip_addr + ":" + tcp_port + "]"

  override def run : Unit = {
    println(my_name + " started and pretending to send.")
    try {
      if(Random.nextGaussian.abs >= 1.5) {
        println(my_name + " will not respond soon.")
        Thread.sleep(10*60*1000) // dead loop connection
      }
      else
        Thread.sleep(Random.nextInt(50*1000) + 50*1000) // 50 to 100 sec
      println(my_name + " peacefully exiting.")
    } catch {
      case e : InterruptedException =>
        println(my_name + " is interrupted.")
    }
  }

  def is_successful : Boolean = (Random.nextInt(10) != 0)
}

class RearrangeTimedMessageRemindActor extends scala.actors.Actor {
  def act : Unit = {
    while(true) {
      // 先发出提醒，再sleep，可以使得每一次启动彩信模块时，对队列的调
      // 整至少执行一次。
      MmsDaemon.remind_rearrange_timed_messages
      Thread.sleep(12*60*60*1000)
    }
  }
}
