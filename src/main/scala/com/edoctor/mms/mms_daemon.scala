package com.edoctor.mms

import net.rubyeye.xmemcached.utils.AddrUtil
import net.rubyeye.xmemcached.{ XMemcachedClientBuilder, MemcachedClient }
import net.rubyeye.xmemcached.command.KestrelCommandFactory

import org.apache.commons.httpclient.{ HttpClient, HttpStatus }
import org.apache.commons.httpclient.methods.GetMethod

import net.tambur.mms.{ MMMessage, MMConstants }

/*
object App {
  def main(args : Array[String]) : Unit = {
    println("Hello world.")
    try {
      MmsDaemon.main_loop
    } finally {
      KestrelHandler.shutdown
    }
    println("Goodbye world.")
  }
} */

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
        e.printStackTrace
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
        e.printStackTrace
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
        e.printStackTrace
        client = initialize_client
        return null
      }
    }

  }

  def shutdown : Unit = client.shutdown
}

class CannotDownloadException(msg : String) extends RuntimeException

object MmsDaemon {
  private val port_config : List[(String, Int, Int, Boolean)] = 
    read_port_config("mms.conf")
/*
List(
    ("192.168.10.230", 964, 200, true),
    ("192.168.10.230", 966, 200, true))
 */

  val ports = 
    port_config.map( p => new ModemPort(p._1, p._2, p._3, p._4) ) 

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
    val type_example = Array("a")
    val fields : Array[String] = (new com.google.gson.Gson).fromJson(request, type_example.getClass)
    val target = fields(0)
    val subject = fields(1)
    val text = fields(2)
    val pic_url = fields(3)
    val mid_url : String = if(fields.length > 4) fields(4) else ""


    Log.debug("make_mms_message", "Got from kestrel queue.")
    Log.debug("make_mms_message", "target is: " + target)
    Log.debug("make_mms_message", "text is: " + text)
    Log.debug("make_mms_message", "pic_url is: " + pic_url)
    Log.debug("make_mms_message", "mid_url is: " + mid_url)
    
    val pic_data = download(pic_url)
    val mid_data = if(mid_url == "") Nil else download(mid_url)

    val msg = new MMMessage()
    msg.setMessageType(MMConstants.MESSAGE_TYPE_M_SEND_REQ)
    msg.setTransactionId("0001")
    msg.setFrom("+8615000279445/TYPE=PLMN")
    msg.setTo("+86" + target + "/TYPE=PLMN")
    msg.setSubject(" " + subject)
    msg.setVersion(1)
    msg.setContentType("application/vnd.wap.multipart.mixed")

    if(pic_data.isEmpty)
      throw new CannotDownloadException(pic_url)
    
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

    msg.encode.toList
  }

  private def download(url : String) : List[Byte] = {
    val httpclient = new HttpClient()
    val httpget = new GetMethod(url)

    try {
      val status_code = httpclient.executeMethod(httpget)

      if(status_code != HttpStatus.SC_OK) 
        Nil
      else
        httpget.getResponseBody.toList
    } finally {
      httpget.releaseConnection
    }
  }

  /*
  private var selector = 
    new SimpleSelector(ports.length)

  private def send(msg : List[Byte]) {
    val port = ports(selector.get)
    if(port.is_ready)
      port.send_mms(msg)
    else {
      port.kill_session
      send(msg)  // 尾递归，取用下一个port
    }
  }
  */

  private def write_conf : Unit = {
    import java.io._
    val filename = "mms.conf"
    val kestrel_info =
      ("# kestrel queue address and port,\n" +
       "# where address and port must be separated with blank, not colon.\n" +
       "kestrel = " + KestrelHandler.queue_addr.replace(':', ' ') + "\n\n")

    val info_string = kestrel_info +
      ("# Following is modem port configuration.\n" +
       "# Each line must start with string \"port=\",\n" +
       "# four data fields are MODEM_IP, MODEM_PORT, DAY_CAPACITY and IS_ENABLED,\n" +
       "# and they must be separated with blank, not comma.\n" +
       "# When system is running, this file will be automatically saved every 2 seconds.\n")

    try {
      val writer = new FileWriter(filename)
      writer.write(info_string + ports.map(_.toString).mkString("\n"))
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
  def get_port : Option[ModemPort] = {
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

  def main_loop : Unit = {
    ports.foreach{p =>
      Log.info("MMS daemon", p.toString) }
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
            val msg : List[Byte] = make_mms_message(request.get)
            port.get.send_mms(request.get, msg)
          }
        }
      } catch {
        case e : CannotDownloadException => {
          Log.error("MMS daemon", e)
          Log.error("MMS daemon",
                    "Cannot download pic " + 
                    e.getMessage + ", message was not sent")
        }
        case e : Exception => {
          Log.error("MMS daemon", e)
          Log.error("MMS daemon", "Above is an overall exception that is not caught in subroutines.")
        }
      }
      Thread.sleep(2000)
      write_conf
    }
  }
}

/*
class SimpleSelector(max : Int) {
  private var counter = 0

  def get : Int = {
    counter = (counter + 1) % max
    counter
  }
}
*/
/**
 * 一个StepSelector在初始化中接受一个权重的表，每次调用get方法时，会得
 * 到一个从0到权重表长减一之间的值，表示选中了表中的某个元素的index。元
 * 素被选中的概率与归一化后的权重相同，而且在满足概率的前提下，能够尽量
 * 避免同一元素被连续选中两次。每次新建StepSelector对象后，第一个选中的
 * 元素是随机的。
 *
 * 算法的原理是使用一个固定的步长去遍历整个权重的区间，如果到头就折返。
 * 权重越高的元素，成为步长落脚处的机会也越多。
 */
/*
class StepSelector(weights : List[Int]) {
  private val accumulated_weights = weights.scanLeft(0)(_ + _)
  private val milestones = accumulated_weights.init
  private val terminal_point = accumulated_weights.last

  // 这个step_len，第一，应当与terminal_point互素，这样才能够遍历所有的
  // 值，才能够精确地达到每个元素被选中的几率的要求；第二，必须大于最大
  // 的weight区间的长度，这样才不会让同一个元素被连续选择2次以上。
  private val step_len = {
    val max_weight = weights.max
    def gcd(a : Int, b : Int) : Int = 
      if(b == 0) a else gcd(b, a % b)
    def find_step_len(trial : Int) : Int = 
      if(gcd(terminal_point, trial) == 1) trial
      else find_step_len(trial + 1)
    find_step_len(max_weight + 1)
  }

  println("step_len = " + step_len)

  private var step_pointer = scala.util.Random.nextInt(terminal_point)

  println("initialized step_pointer = " + step_pointer)

  def get : Int = {
    val selected = milestones.lastIndexWhere(_ <= step_pointer)     
    step_pointer = (step_pointer + step_len) % terminal_point
    selected
  }
}
*/

// 一个ModemPort对象的生存期贯穿从彩信模块开始运行到结束运行，与之相
// 对，ModemSession对象则是为每条彩信建立一个。当ModemPort对象刚刚完成
// 初始化的时候，它的capacity和enabled参数来自于构造函数，而my_session
// 成员变量为null。
class ModemPort(the_ip_addr : String, the_tcp_port : Int, the_capacity : Int, the_enabled : Boolean) {
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

/*
    if(!enabled)
      false
    else if(!cooled_down)
      false
    else if(my_session == null)
      true
    else if(my_session.getState == Thread.State.TERMINATED) {
      push_to_history(my_session.is_successful)
      my_session = null
      true
    }
    else false
*/

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
    ("port = " + 
     ip_addr + "  " + 
     tcp_port + "     " + 
     my_capacity + "     " + 
     enabled)
}

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
