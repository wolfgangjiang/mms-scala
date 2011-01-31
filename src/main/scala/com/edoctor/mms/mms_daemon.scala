package com.edoctor.mms

import net.rubyeye.xmemcached.utils.AddrUtil
import net.rubyeye.xmemcached.{ XMemcachedClientBuilder, MemcachedClient }
import net.rubyeye.xmemcached.command.KestrelCommandFactory

import org.apache.commons.httpclient.{ HttpClient, HttpStatus }
import org.apache.commons.httpclient.methods.GetMethod

import net.tambur.mms.{ MMMessage, MMConstants }

// 若一个port连续不能成功发送，可以考虑降低它的capacity，或者将它
// disable半小时左右，让modem休息一下，也许接下去就行了。如果还是不行，
// 则可能是余额不足。封号是无法用端口来探知的，如果封号，是会在端口wsp
// 报告成功发送（即返回200 OK）后，才来一条文字短信，告诉今天超过了限额。
// 而且这条被封号后发送的彩信就此丢失，移动服务商不会补发。

// KestrelHandler模块为整个提供对kestrel队列的读写服务。队列的ip地址和
// 端口号由配置文件"mms.conf"中给出，写在"kestrel ="开头的一行中，ip地
// 址和端口号用空格隔开。以后改进时，应当考虑将相关的import语句都移动到
// 类的内部来。
object KestrelHandler {
  val queue_addr = read_kestrel_config("mms.conf")
  private var i_am_fine = true

  // 变量i_am_fine和方法is_fine指出最近一次对于kestrel队列的访问是否成
  // 功。
  def is_fine = i_am_fine

  // 从配置文件中读取kestrel队列的ip地址和网络端口。kestrel队列的端口通
  // 常是22133，但是也可以设置成其它数字。在我们配置文件中，地址和端口
  // 号应当用空格隔开，这是为了与短信猫的ip地址、端口格式相统一。但是在
  // xmemcached库中，则是默认用冒号隔开ip地址和端口号，所以在函数
  // read_kestrel_config中，将会增添分隔的冒号。 如果在配置文件中不包含
  // “kestrel =”开头的一行，会在本函数内Option.get方法（不是我们这个模
  // 块里的get方法）上抛异常。如果给出了用冒号分隔的地址，倒也能正常通
  // 过，因为那样的话，split就不会起作用，得到的只是一个单一元素的
  // List，它的mkString也就不会作任何附加的操作，只会返回那个单一的元素。
  // 如果ip地址和端口号完全错误，那么这个错误的字符串会记在
  // queue_addr里，导致xmemcached无法连接kestrel队列，令i_am_fine为
  // false，从而通过i_am_fine的渠道报告异常。函数read_kestrel_config仅
  // 在程序启动时运行一次，之后不会再运行。
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

  // 它是对kestrel队列API的核心。对队列的读取要调用client.get，对队列的
  // 写入要调用client.set。
  private var client : MemcachedClient = initialize_client

  // 函数initialize_client使用了queue_addr成员变量，如果queue_addr中的
  // 地址是错误的，就会在此处抛出异常，置i_am_fine为假。除了在程序启动
  // 时本函数被用来初始化之外，每一次get或put中出现异常时，都会调用本函
  // 数，这样，如果kestrel队列暂时不工作，后来又修好了，可以不重启彩信
  // 模块，而自动地找到修好后的kestrel队列。当然，目前queue_addr仅仅在
  // 程序启动时初始化一次，所以如果配置文件是胡乱写的，那么除了重启彩信
  // 模块之外别无他法。
  private def initialize_client : MemcachedClient = 
    try {
      val the_builder = 
        new XMemcachedClientBuilder(AddrUtil.getAddresses(queue_addr))
      the_builder.setCommandFactory(new KestrelCommandFactory())
      val the_client = the_builder.build
      the_client.setPrimitiveAsString(true) // 字符串前不附加flag
      // 一旦字符串前不附加flag，就无法用kestrel队列传递其它类型的信
      // 息，只能传递字符串了——这正合我们之意。我们仅仅需要传递字符串。
      the_client.setOptimizeGet(false) // 因为kestrel不支持bulk get
      // 关于bulk get的详情请查询memcached文档。
      the_client // 返回值，如果成功运行到这一行，则i_am_fine默认为true。
    } catch {
      case e : Exception => {
        i_am_fine = false
        Log.error("KestrelHandler", e)
        return null
      }
    }

  // 如果队列中有内容，不为空，则返回该取出的那个元素。如果队列为空，什
  // 么元素也没有，则返回null。队列中不可能有一个元素是null，所以不会出
  // 现歧义，因为kestrel队列本身不允许把null放入队列中充当一个元素。以
  // 后应该可以考虑把它做成是返回Optional[String]类型的，这样更符合
  // scala的风格。如果在读取时抛出异常，则置i_am_fine为假，而且试图重新
  // 地初始化。我们的队列名写死为“mms”，以后如果要改进，应该将它改成可
  // 配置的。
  def get : String = {
    try {
      val value : String = client.get("mms")
      i_am_fine = true
      value
    } catch {
      case e : Exception => {
        i_am_fine = false 
        Log.error("KestrelHandler", e)
        // 不要在此处调用initialize_client。已有的client能够自动识别
        // 重新启动的队列。如果在这里调用initialized_client，而不先将
        // 已有的client 进行shutdown，反而会耗尽socket连接句柄，造成
        // too many open files异常。
//        client = initialize_client
        return null
      }
    }
  }

  // 将字符串放入队列中，注意队列的名字写死为"mms"。其它与get方法大同小异，
  def put(thing : String) : Unit = {
    try {
      // 这里的第二个参数是memcache的expiration值，“0”表示无期限长期存
      // 放。
      client.set("mms", 0, thing) 
      i_am_fine = true
    } catch {
      case e : Exception => {
        i_am_fine = false 
        Log.error("KestrelHandler", e)
        // 不要在此处调用initialize_client，参见get()方法对此的注释。
//        client = initialize_client
      }
    }
  }

  // 在彩信模块退出前，应当显示地调用本方法。如果不shutdown就强制退出，
  // 会打开的文件（或socket句柄）未释放。
  def shutdown : Unit = client.shutdown

  // 这个函数对整个mms队列进行重新排序，将定时在当天的彩信请求放到队列
  // 的最前部，最先取出这些彩信请求，将其它的彩信请求按照原先的次序放到
  // 队列后面。
  def rearrange_timed_messages : Unit = {
    // today_string的格式类似于当天的"19970530"。这个计算的过程也许写成
    // 一个utility函数更加合适一些。
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
          going = false  // 把整个队列的内容都取出来，清空为止。
        } else {
          val type_example = Array("a") 
          // 仅仅是为了弄一个Array[String]型对象，我们仅仅关心它的类
          // 型，不关心它的值。这个Json的操作，也是以弄成一个utility函
          // 数为宜。
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
        // 这个异常处理出现了三次，也许也可以写一个函数来精炼之。
        i_am_fine = false 
        Log.error("KestrelHandler", e)
        client = initialize_client
        return null
      }
    }
  }
}

// 一般在将彩信请求制作成MIME格式的彩信的过程中可能抛出这个异常，包括本
// 身的格式异常，也包括从给定的网址无法获取图片等网络异常。
class MalformedMessageException extends RuntimeException


// MmsDaemon对象处理各个端口的调度，定时检查kestrel队列中的短信请求，将
// 短信请求递送给合适的空闲端口，
object MmsDaemon {
  private val port_config : List[(String, Int, Int, Boolean)] = 
    read_port_config("mms.conf")
  private var remind_rearrange : Boolean = false

  // 本函数由一个自动定时的actor调用，每隔固定时间调用一次。当主循环看
  // 到remind为真时，执行预定操作，并且随即将remind置为假，等待下一次
  // remind。也就是说，remind被写两次，总是在本函数中置真，在主循环中置
  // 假。
  def remind_rearrange_timed_messages : Unit = { remind_rearrange = true }

  // 这里，p._1、p._2、p._3、p._4分别代表ip地址、udp端口、capacity和
  // is_enabled值，可参见write_conf函数。它必须与write_conf保持一致。
  // sp端口也就是特服号端口的capacity和is_enabled是写死的，以后要改成可
  // 配置的。当然，sp端口本身的逻辑难以在配置文件中表示。
  val ports : List[Port] = 
    (port_config.map( p => new ModemPort(p._1, p._2, p._3, p._4) ) :+ 
     new SPPort("Cosms SP        ", 1440, false, CosmsSPInfo.send_mms_method))

    // 本函数仅仅在程序启动时运行一次，如果发现配置文件读取有错误，就会
    // 将程序结束，不会启动主循环。
  private def read_port_config(filename : String) : List[(String, Int, Int, Boolean)] = {
    // 这个find_port很可能抛出越界异常以及数据格式异常，这些异常将被下
    // 面的try语句接住。
    def find_port(line : String) : Option[(String, Int, Int, Boolean)] =
      if(line.startsWith("port =")) {
        val data = line.drop(6).trim.split("\\s+")
        Some((data(0), data(1).toInt, data(2).toInt, (data(3) == "true")))
      } else if(line.startsWith("port=")) {
        val data = line.drop(5).trim.split("\\s+")
        Some((data(0), data(1).toInt, data(2).toInt, (data(3) == "true")))
      } else None

    try {
      // 把None都filter掉，只留下Some，然后get之。
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

  // 从彩信请求制作二进制的彩信。它包括以下步骤：(1)取出请求中的各个字
  // 段；(2)下载请求中指定的图片和音乐；(3)填充彩信为tambur的MMMessage
  // 对象；(4)调用MMMessage.encode，得到二进制的彩信表示。也许这个以后
  // 应当放到Port或ModemPort中，不放在主循环的线程里，而放到相应彩信的
  // 线程里。
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
      // text直接写入彩信，pic和midi要从给定的url下载，然后写入彩信。

      Log.debug("make_mms_message", "Got from kestrel queue.")
      Log.debug("make_mms_message", "target is: " + target)
      Log.debug("make_mms_message", "text is: " + text)
      Log.debug("make_mms_message", "pic_url is: " + pic_url)
      Log.debug("make_mms_message", "mid_url is: " + mid_url)

      // 在这里无法应对pic_url或mid_url为null的情况，不过download抛出一
      // 个异常之后，仍然能够返回空List即Nil，所以还好，彩信的其余部分
      // 仍然可以正常发送。
      Log.debug("make_mms_message", "Trying to download pic at " + pic_url)
      val pic_data = if(pic_url == "") Nil else download(pic_url)
      Log.debug("make_mms_message", "Pic downloaded. Now trying to download mid at " + mid_url)
      val mid_data = if(mid_url == "") Nil else download(mid_url)
      Log.debug("make_mms_message", "Downloading finished, ")

      val msg = new MMMessage()
      msg.setMessageType(MMConstants.MESSAGE_TYPE_M_SEND_REQ)
      msg.setTransactionId("0001")
      msg.setFrom("+8615000279445")
      msg.setTo("+86" + target + "/TYPE=PLMN")
      msg.setSubject(" " + subject) // 如果前缀不加一个空格，就无法显示中文。
      // 上面，对Subject，ascii字符可以显示，ascii字符后跟中文也可以显示，但
      // 中文开头就是不能显示。所以要加一个空格。
      msg.setVersion(1)
      msg.setContentType("application/vnd.wap.multipart.mixed")

      if(!pic_data.isEmpty) // isEmpty也就是空表，也就是Nil，也就是List()
        msg.addPart("image/gif; name=\"imagepart.gif\"", pic_data.toArray, false, null, null)
      // 全部当作gif发送，这是不合理的。以后要把它变得更加泛用化，适应
      // 更多的图片格式。name参数是必要的，如果不这样，那么除了Nokia之
      // 外，各种手机都不能正常显示图片。

      msg.addPart("text/plain; charset=\"utf-8\"", (" " + text).getBytes("utf-8"), false, null, null) // String.getBytes返回的默认是utf-8编码。

      if(!mid_data.isEmpty)
        msg.addPart("audio/midi; autostart=true", mid_data.toArray, false, null, null)  // autostart参数看起来没有什么用处。

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

  // 使用apache的httpclient从给定的url下载文件，将下载到的文件以二进制
  // 形式返回。它用来下载pic、midi等二进制文件。
  private def download(url : String) : List[Byte] = {
    Log.debug("download", "downloading " + url)

    val httpclient = new HttpClient()
    val httpget = new GetMethod(url)

    // 在这里必须手动设置timeout值，以毫秒为单位。如果不进行这样的设
    // 置，则对于响应慢的网站，可能导致整个彩信模块卡死在这里的等待中。
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
        // 对于httpclient的任何异常，都在这里接住，返回空表，表示什么也
        // 没有下载到。
        Log.error("download", e)
        return Nil
      }
    } finally {
      httpget.releaseConnection
    }
  }

  // 这个write_conf是主循环的每次循环都调用一次的，实时地储存当时的状态
  // 设置，主要是capacity与is_enabled的值。写入的conf文件就是程序启动时
  // 读取的conf文件，在下次启动时可以直接读取。不过这里对配置文件名字的
  // 赋值在以后应该统一到同一个地方去。import语句也要重新写好，还有主循
  // 环的延时2秒，也要一直保持和主循环的实际延时一致，也应该写到同一个
  // 地方去。
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

    var writer : FileWriter = null
    try {
      writer = new FileWriter(filename)
      writer.write(info_string + ports.map(_.to_conf_string).mkString("\n"))
      writer.close
    } catch {
      case e : IOException => {
        Log.error("write_conf", e)
        Log.error("write_conf", "Error when auto writing to conf file" + filename)
      }
    } finally {
      try {
        writer.close
      } catch {
        case e : Exception => { }
      }
    }
    writer = null
  }

  
  // 在这里可以对ports进行多次操作，不必担心同步问题，因为除了本对象之
  // 外，不会有其它线程使得一个ready的port变成“不ready”的。 先把停滞时
  // 间太长的port给杀掉，让它们变成ready的，然后从所有ready的中随机选择
  // 一个。当然那，也可能选不到，也就是当前所有的port都是正当地忙碌着。
  def get_port : Option[Port] = {
    ports.foreach(_.kill_dangling_session)
    val ready_ports = ports.filter(_.is_ready)
    if(ready_ports.isEmpty)
      None
    else
      Some(ready_ports(util.Random.nextInt(ready_ports.length)))
  }

  // 对KestrelHandler.get进行简单的Option包装。这个其实可以挪动到
  // KestrelHandler中。
  def get_request : Option[String] = {
    val request = KestrelHandler.get
    if(request == null)
      None
    else
      Some(request)
  }

  // 对于定时在未来的彩信请求，返回true，以便拒绝在当前发送。这个判断应
  // 当是非常严格的，很难返回true。原则是，宁可错误地提前发送，也不要将
  // 本该现在发送的加以拖延，因为一旦错误地加以拖延，就可能永远也等不到
  // “发送的那一天”，这个彩信就会永远停留在kestrel队列里了。也许我们以
  // 后应该改得比现在更严格，包括要求所有字符都是数字，以及长度为8，乃
  // 至它必须能通过SimpleDateFormat.parse()等。
  private def is_of_future(request : String) : Boolean = {
    // today_string的格式类似于当天的"19970530"。这里的时间格式语句与
    // KestrelHandler中重复了，以后应当将它们统一为一个utility函数。
    val today_string : String = 
      (new java.text.SimpleDateFormat("yyyyMMdd")).format(
        java.util.Calendar.getInstance.getTime) 
    
    try {
      // 对Json的处理，以后应该写一个utility函数。
      val type_example = Array("a")
      val fields : Array[String] = (new com.google.gson.Gson).fromJson(request, type_example.getClass)
      ((!fields.isEmpty) &&   // 考虑到&&是短路操作，应该不会抛出数组越界异常
       (!fields(0).isEmpty) &&
       (fields(0)(0).isDigit) &&
       (fields(0) > today_string))
    } catch {
      case e : Exception => 
        return false
    }
  } // is_of_future()

  def main_loop : Unit = {
    // 这在程序启动的时候仅仅运行一回，用来告知管理员，自己在配置文件中
    // 读到了什么。
    ports.foreach{p =>
      Log.info("MMS daemon", p.to_conf_string) }
    var going = true // 目前没有看到有什么能够在主循环里令它为false，暂
                     // 时留着，看以后会不会有需要用到它的时候。
    Log.info("MMS daemon", "Main loop started")
    while(going) {  // daemon的主循环
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
              // 以上放入队尾也没有关系，到了该发送它的那一天，它总是会
              // 被rearrange到队列前部，优先发送的。
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
        write_conf  // 覆盖写入配置文件，以当前内存中的配置为准。
        if(remind_rearrange) { // 用于定时发送
          remind_rearrange = false
          KestrelHandler.rearrange_timed_messages
          // 在进行上述的队列调整时，对队列的其它操作基本上是停下来的。
          // 只有零星的并发可能是ModemPort将发送失败的彩信请求放回到队
          // 列里，kestrel队列应该可以完善应付这种小的并发请求而不至于
          // 崩溃。而且，无论如何，定时在未来的请求总也不会提前被发送。
        }
      } catch {
        case e : MalformedMessageException => {
          Log.error("MMS daemon", e)
        }
        case e : Exception => {
          // 这里是保护主循环不崩溃退出的最后一道防线。
          Log.error("MMS daemon", e)
          Log.error("MMS daemon", "Above is an overall exception that is not caught in subroutines.")
        }
      } // catch
    } // while
  } // main_loop
}

// trait Port是ModemPort和SPPort的共同父类，被MmsDaemon直接使用。这里定
// 义的函数都是要在MmsDaemon中用到的。其中颇有一些函数在ModemPort和
// SPPort中相同，以后应当集中写到trait中来。
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
  // send_mms()是发送彩信的主入口
  def send_mms(request : String, msg_data : List[Byte]) : Unit
  def to_conf_string : String
}

// ModemPort类刻画短信猫上装的一个sim卡。一个ModemPort对象的生存期贯穿
// 从彩信模块开始运行到结束运行，与之相对，ModemSession对象则是为每条彩
// 信建立一个。当ModemPort对象刚刚完成初始化的时候，它的capacity和
// enabled参数来自于构造函数，而my_session成员变量初始化为null。
// my_session为null便令它进入ready状态。
class ModemPort(the_ip_addr : String, the_tcp_port : Int, the_capacity : Int, the_enabled : Boolean) extends Port {
  val ip_addr = the_ip_addr
  val tcp_port = the_tcp_port
  val capacity_list = List(10,50,100,200,480,720)
  // capacity表示每天能够发送彩信的条数，因为它是用interval_of_send来维
  // 持的，所以哪怕系统重启，也肯定不会超过一天内指定的条数。

  // interval_of_send只不过是从另一个角度来看待capacity_list。但也可能
  // 需要给my_capacity为0的可能性加上一重保护。
  def interval_of_send = (1440 / my_capacity) * 60 * 1000L

  private var my_last_sent_time : Long = 0L // 这个初始值使得第一条必然
                                            // 不会因为冷却而无法发送。
  // 与modem（短信猫）进行交互的session，它是一个子线程。
  private var my_session : MmsSession = null 
  // private var my_session : DummySession = null
  // DummySession是测试用的。

  // history仅仅记录是成功还是失败，这个history在彩信模块重启后就没有
  // 了，所以不能取代log的作用。
  private var history = List[Boolean]() 
  private var enabled = the_enabled
  // 如果给定的the_capacity不在合法值的表中（这是有可能的，因为初始化用
  // 的the_capacity一般来自于人类写的配置文件），那么在合法值的表中找到
  // 一个不超过它的、最大的值，作为初始值。以后change_capacity就只会在
  // 合法值中切换，不会超出范围了。这样，至少它不会是0，也就不至于在
  // interval_of_send中引发除0异常了。
  private var my_capacity = 
    capacity_list.reverse.find(x => (x <= the_capacity)) match {
      case Some(x) => x
      case None => 50
    }

  // 这个用于在请求失败时，将request送回到kestrel队列中。
  private var last_request : String = null

  private def push_to_history(value : Boolean) : Unit = {
    history = value :: history
    if(history.length > 10)
      history = history.take(10) 
    // 仅仅记录10个历史。这个数值在以后不妨改成可配置的。之所以仅仅记录
    // 10个历史，是因为这样可以对一个端口最近的健康状况提供参考。
    if(!value)
      KestrelHandler.put(last_request) // 这里是唯一用到last_request的
    // 地方。如果kestrel队列里只有一个彩信请求，而且总是发送失败，那么
    // 这个唯一的彩信请求就会轮转到多个端口，如果这些端口还是失败，就会
    // 把多个副本送回到队列中，而如果下一次各个端口都成功了，用户就会重
    // 复收到多个同样彩信。不过这只是个小bug。
  }

  // 它是用静态记录与客观时间计算出来的，所以相当牢靠，不会因为enabled
  // 的切换而改变，也不会change_capacity中的capacity变化而受到影响。
  private def cooled_down : Boolean = 
    (System.currentTimeMillis - my_last_sent_time > interval_of_send)

    // 本函数将history中的比例变成更易读的表示
  def success_rate_string : String = 
    if(history.isEmpty) "No Record"
    else 
      (history.count(x => x).toString + "/" +
       history.length.toString + " == " +
       (history.count(x => x) * 100 / history.length).toString + "%") 

      // 本函数用处不大，不过删掉它倒也会有小小的不便。那么就不妨为了小
      // 小的方便而留着它。
  def is_ready : Boolean = 
    (get_status_string == "Ready")

    // 在telnet控制界面按回车键来改变capacity时，就是调用本函数。
    // capacity只能在几个预设值之间改变。
  def change_capacity : Unit = {
    val place = capacity_list.indexOf(my_capacity)
    val new_place = 
      if(place < 0) 0  // 如果在capacity_list中找不到，indexOf会返回-1。
      else if(place + 1 >= capacity_list.length) 0 // 如果超出最大
      else place + 1
    my_capacity = capacity_list(new_place)
  }

  // 外界用本函数来读取capacity。
  def capacity = my_capacity

  // 这是显示在telnet界面的状态字符串，分成ready、busy、coolingDown和
  // disabled四类。对于busy，显示在busy状态中已经停留的时间，这个时间是
  // 不断增加的；对于cool down，显示的确实还剩的冷却时间，这个时间是倒
  // 计时，是不断减少的。telnet控制界面每次刷新都会调用本函数，而本函数
  // 会清扫线程状态为TERMINATED的线程，释放之，将相应的my_session指针置
  // 为null。所以这种清扫是频繁的。但是，以后为了代码的健康，可能还是把
  // 这种清扫挪动到主循环中更加合适。
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
      my_session.interrupt // TERMINATED的线程也可以发送interrupt，不会
                           // 有异常，也不作任何反应。
      my_session = null
      push_to_history(false)
    }
    enabled = !enabled
  }

  def is_enabled : Boolean = enabled

  // 只有enabled且cooled_down而且还忙着的情况下，才视为dangling
  // session，也就是忙了太长时间的线程。
  def kill_dangling_session : Unit = 
    if((my_session != null) && cooled_down && enabled) {
      my_session.interrupt 
      my_session = null 
      push_to_history(false)
    }

  // 这是发送一条彩信的主入口。
  def send_mms(request : String, msg_data : List[Byte]) : Unit = {
    if(is_ready) {
      Log.info(this.toString, "sending mms")
      my_session = new MmsSession(ip_addr, tcp_port, msg_data)
//    my_session = new DummySession(ip_addr, tcp_port, msg_data)
      // DummySession是daemon测试用的。
      my_session.start // 线程开始
      my_last_sent_time = System.currentTimeMillis
      last_request = request
    } else 
      Log.error(this.toString, "Bug, trying to send msg via a port that is not ready.")
  }

  // 这个toString会被telnet控制界面和log调用。所以表示的不是它的巨细无
  // 遗的内部信息，而是它本身的静态唯一标识。
  override def toString : String = 
    (ip_addr + " " + tcp_port)    // 为了打印整齐，长度必须为19

    // 这个conf_string将被写入配置文件
  def to_conf_string : String =
    ("port = " + 
     ip_addr + "  " + 
     tcp_port + "     " + 
     my_capacity + "     " + 
     enabled)
}


// SPPort刻画一个特服号的提供商API。参数中，send_mms_method刻画特服号的
// 具体API，它的字符串参数是request，二进制参数是彩信内容，而返回值表示
// 是否成功。有时特服号是直接在request层面上操作的，不需要彩信的二进制
// 内容，所以作为统一界面，request和二进制内容都传送给它。
class SPPort(the_name : String, the_capacity : Int, the_enabled: Boolean, send_mms_method : (String, List[Byte]) => Boolean) extends Port {
  val my_name = the_name // 端口的唯一标识，通常也就是特服号的唯一标识
  val capacity_list = List(10,50,100,200,480,720,1440)
  // 它的capacity比ModemPort要宽松，ModemPort最快也得2分钟发送一条，而
  // SPPort只需要考虑一天发送不要超过1千多条即可。

  // 与ModemPort相同
  def interval_of_send = (1440 / my_capacity) * 60 * 1000L

  private var my_last_sent_time : Long = 0L // 与ModemPort相同
  private var history = List[Boolean]() // 与ModemPort相同
  private var enabled = the_enabled // 与ModemPort相同
  private var my_capacity =  // 与ModemPort相同
    capacity_list.reverse.find(x => x <= the_capacity) match {
      case Some(x) => x
      case None => 50
    }

  private var last_request : String = null // 与ModemPort相同

  // 与ModemPort相同
  private def push_to_history(value : Boolean) : Unit = {
    history = value :: history
    if(history.length > 10)
      history = history.take(10)
    if(!value)
      KestrelHandler.put(last_request)
  }

  // 与ModemPort相同
  private def cooled_down : Boolean = 
    (System.currentTimeMillis - my_last_sent_time > interval_of_send)

    // 与ModemPort相同
  def success_rate_string : String = 
    if(history.isEmpty) "No Record"
    else 
      (history.count(x => x).toString + "/" +
       history.length.toString + " == " +
       (history.count(x => x) * 100 / history.length).toString + "%") 

      // 与ModemPort相同
  def is_ready : Boolean = 
    (get_status_string == "Ready")

    // 函数与ModemPort相同，虽然capacity_list的内容可能不同。
  def change_capacity : Unit = {
    val place = capacity_list.indexOf(my_capacity)
    val new_place = 
      if(place < 0) 0
      else if(place + 1 >= capacity_list.length) 0
      else place + 1
    my_capacity = capacity_list(new_place)
  }

  def capacity = my_capacity

  // 用于显示在telnet控制界面的字符串。与ModemPort不同。在SPPort中其实
  // 并没有busy的概念，在发送之后直接进入cooldown。但是cooldown的倒计时
  // 是相同的。
  def get_status_string : String = {
    def cooling_down_string : String = {
      val time = (interval_of_send - (System.currentTimeMillis - my_last_sent_time))
        ("Cool Down(" +
         (time / 60 / 1000).toString + ":" +
         ((time / 1000) % 60).toString + ")"
       )
    }

    if(!enabled)
      "Disabled"
    else
      if(cooled_down) "Ready" else cooling_down_string
  }
  
  // 与ModemPort不同，因为没有busy的概念，所以也谈不上强行切断。
  def toggle_enabled : Unit = {
    enabled = !enabled
  }

  // 与ModemPort相同
  def is_enabled : Boolean = enabled

  // 与ModemPort不同
  def kill_dangling_session : Unit = { } // do nothing

  // 与ModemPort不同，不起子线程，直接调用send_mms_method。
  def send_mms(request : String, msg_data : List[Byte]) : Unit = {
    Log.info(this.toString, "sending mms")
    last_request = request
    my_last_sent_time = System.currentTimeMillis
    push_to_history(send_mms_method(request,msg_data))
    Log.info(this.toString, "finished sending")
  }

  // 与ModemPort不同
  override def toString : String = my_name

  // 与ModemPort不同
  def to_conf_string : String =
    ("SP   = " + 
     my_name + "     " + 
     my_capacity + "     " + 
     enabled)
}

// 测试用，它提供的对外接口与ModemSession相同。
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
        Thread.sleep(10*60*1000) // dead loop connection，
        // 刻画长时间不应答的session，用来测试daemon杀掉这些session的能力。
      }
      else // 正常的session，发送一条彩信大约需要50-100秒。
        Thread.sleep(Random.nextInt(50*1000) + 50*1000) // 50 to 100 sec
      println(my_name + " peacefully exiting.")
    } catch {
      case e : InterruptedException =>
        println(my_name + " is interrupted.")
    }
  }

  def is_successful : Boolean = (Random.nextInt(10) != 0)
}

// 类似于Timer类的一个小工具，它应该在整个彩信模块刚刚开始运行时启动。
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
