package com.edoctor.mms

import net.rubyeye.xmemcached.utils.AddrUtil
import net.rubyeye.xmemcached.{XMemcachedClientBuilder, MemcachedClient}
import net.rubyeye.xmemcached.command.KestrelCommandFactory

import org.apache.commons.httpclient.{ HttpClient, HttpStatus }
import org.apache.commons.httpclient.methods.GetMethod

import net.tambur.mms.{ MMMessage, MMConstants }


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
}

object KestrelHandler {
  val queue_addr = "127.0.0.1:22133"
  
  private val client : MemcachedClient = {
    val the_builder = 
      new XMemcachedClientBuilder(AddrUtil.getAddresses(queue_addr))
    the_builder.setCommandFactory(new KestrelCommandFactory())
    val the_client = the_builder.build
    the_client.setPrimitiveAsString(true) // 字符串前不附加flag
    the_client.setOptimizeGet(false) // 因为kestrel不支持bulk get
    the_client
  }

  def get : String = client.get("mms")

  def shutdown : Unit = client.shutdown
}

class CannotDownloadException(msg : String) extends RuntimeException

object MmsDaemon {
  private val port_config : List[(String, Int)] = List(
    ("192.168.10.230", 964),
    ("192.168.10.230", 966))

  private val ports = 
    port_config.map( p => new ModemPort(p._1, p._2) ) 

  private def make_mms_message(request : String) : List[Byte] = {
    val fields = request.split("\\s+")
    val target = fields(0)
    val subject = fields(1)
    val text = fields(2)
    val pic_url = fields(3)
    val mid_url = if(fields.length > 4) fields(4) else "none"

    println("target is: " + target)
    println("text is: " + text)
    println("pic_url is: " + pic_url)
    println("mid_url is: " + mid_url)

    val pic_data = download(pic_url)
    val mid_data = if(mid_url == "none") Nil else download(mid_url)

    val msg = new MMMessage()
    msg.setMessageType(MMConstants.MESSAGE_TYPE_M_SEND_REQ)
    msg.setTransactionId("0001")
    msg.setFrom("+8615000279445/TYPE=PLMN")
    msg.setTo("+86" + target + "/TYPE=PLMN")
    msg.setSubject(" " + subject)
    msg.setVersion(1)
    msg.setContentType("application/vnd.wap.multipart.related")

    if(pic_data.isEmpty)
      throw new CannotDownloadException(pic_url)
    
    msg.addPart("image/gif", pic_data.toArray, false, null, null)

    val test_s = new java.io.DataOutputStream(
      new java.io.FileOutputStream(new java.io.File("a.gif")))
    test_s.write(pic_data.toArray, 0, pic_data.length)
    test_s.close

    val test_s2 = new java.io.DataOutputStream(
      new java.io.FileOutputStream(new java.io.File("a.mid")))
    test_s2.write(mid_data.toArray, 0, mid_data.length)
    test_s2.close

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

  private var selector = 
    new StepSelector(ports.map(_.weight))

  private def send(msg : List[Byte]) {
    val port = ports(selector.get)
    if(port.is_ready)
      port.send_mms(msg)
    else {
      port.kill_session
      send(msg)  // 尾递归，取用下一个port
    }
  }

  def main_loop : Unit = {
    println(ports)
    var going = true
    while(going) {
      val request = KestrelHandler.get
      if(request != null) {
        try {
          val msg : List[Byte] = make_mms_message(request)
          send(msg)
          return
        } catch {
          case e : CannotDownloadException =>
            println("Cannot download pic " + 
                    e.getMessage + ", message was not sent")
        }
      }
      // Thread.sleep(2000)
    }
  }
}

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


class ModemPort(ip_addr : String, tcp_port : Int) {
  val interval_of_send = 60*1000L

  private var my_weight = 100
  private var my_last_sent_time : Long = 0L
  private var my_session : MmsSession = null
  private var history = List[Boolean]()

  def weight = my_weight
  def set_weight(value : Int) { my_weight = value }

  private def push_to_history(value : Boolean) : Unit = {
    history = value :: history
    if(history.length > 100)
      history = history.take(100)
  }

  def success_rate : Double = 
    if(history.isEmpty)
      0.0
    else
      history.count(x => x).toDouble / history.length

  def is_ready : Boolean = 
    if(System.currentTimeMillis - my_last_sent_time < interval_of_send)
      false
    else if(my_session == null)
      true
    else if(my_session.getState == Thread.State.TERMINATED) {
      push_to_history(my_session.is_successful)
      my_session = null
      true
    }
    else false

  
  def kill_session : Unit = { 
    if(my_session != null) {
      my_session.interrupt 
      my_session = null 
      push_to_history(false)
    }
  }

  def send_mms(msg_data : List[Byte]) : Unit = {
    my_session = new MmsSession(ip_addr, tcp_port, msg_data)
    my_session.start
    my_last_sent_time = System.currentTimeMillis
  }
}
