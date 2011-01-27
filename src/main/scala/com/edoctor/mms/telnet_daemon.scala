package com.edoctor.mms

import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.shell.Shell
import net.wimpi.telnetd.net.{ Connection, ConnectionEvent }
import net.wimpi.telnetd.io.BasicTerminalIO

// 这里是整个彩信模块的应用程序入口。
object App {
  // 可以用于load各种java properties，不过在本程序中主要是为了给
  // telnetd使用。
  private def load_properties(filename : String) : java.util.Properties = {
    import java.util.Properties
    import java.io.BufferedInputStream
    import java.io.FileInputStream

    val in_s = new BufferedInputStream(new FileInputStream(filename))
    val prop = new Properties()
    prop.load(in_s)
    prop
  }

  def main(args : Array[String]) : Unit = {
    println("Hello world.")
    val telnet_daemon = 
      TelnetD.createTelnetD(load_properties("telnetd.properties"))

    telnet_daemon.start    
    (new RearrangeTimedMessageRemindActor).start  // 提醒mms daemon整理
                                                  // 队列中的定时彩信请求

    try {
      MmsDaemon.main_loop
    } finally {
      KestrelHandler.shutdown
    }
    /*
    val source = io.Source.stdin
    println("Enter \"quit\" to shutdown.")
    print("mms>")
    source.getLines.foreach{
      line => {    
        if(line == "quit") {
          telnet_daemon.stop
          exit(0)
        }
        else println("huh?")
        print("mms>")
      }
    }*/

    telnet_daemon.stop
  }
}

// 这是telnetd开源库所规定的回调入口。它提供io，使得我们可以用这个io来
// 输入输出，从服务器端处理telnet连接请求。
class TelnetShell extends Shell {
  private object ShellMode extends Enumeration {
    type ShellMode = Value  // 我们分成若干个mode来显示。
    val M_status, M_capacity, M_history = Value
  }  
  import ShellMode._
  
  private var my_connection : Connection = null
  private var my_io : BasicTerminalIO = null
  private val timer_task = new TelnetRefreshTimerTask(this)
  private var mode : ShellMode = M_status  // 初始的mode
  private var caret = 0  // 光标

  def run(conn : Connection) : Unit = {
    my_connection = conn
    my_io = conn.getTerminalIO

    OurTimer.schedule(timer_task, 0, 500) // refresh用，每500毫秒1次

    my_connection.addConnectionListener(this) // telnetd开源库规定必须这样做

    /*
    my_io.synchronized {
      my_io.eraseScreen
      my_io.homeCursor
      my_io.write("I am your shell and I say hello.\n")
      my_io.write("你好。\n")
      my_io.flush
    } */

    var going = true
    while(going) {
      import BasicTerminalIO._
      val key = my_io.read   // 阻塞。自动refresh来自另一个线程（Timer线程）
      if(key == LEFT && 
         caret % 2 == 1) caret -= 1  // 移动光标
      else if(key == RIGHT && 
              caret % 2 == 0 
              && caret < MmsDaemon.ports.length - 1) caret += 1
      else if(key == UP &&
              caret >= 2) caret -= 2
      else if(key == DOWN &&
              caret < MmsDaemon.ports.length - 2) caret += 2
      else if(key == ENTER) mode match { // 回车键在不同的模式意味不同的操作
        case M_status => MmsDaemon.ports(caret).toggle_enabled
        case M_capacity => MmsDaemon.ports(caret).change_capacity
        case M_history => { }
      }
      else key.toChar match {
        case '1' => mode = M_status  // 1、2、3键都是切换模式
        case '2' => mode = M_capacity 
        case '3' => mode = M_history 
        case 'q' => going = false  // 退出
        case 'Q' => going = false
        case _ => { }  // do nothing
      }
      scheduled_refresh // 有键盘输入立刻refresh，让用户立刻看到控制的
                        // 结果，因为我们这个线程是阻塞的，所以没有键盘
                        // 输入就不会运行到这里。
    }

    timer_task.cancel  // 不cancel会造成内存泄漏。这里只是即将退出
                       // telnet shell，而系统运行期间很可能会有多次
                       // telnet登录。
// 多个线程同时写io时，如果不用synchronized上锁，就会造成telnetd崩溃
    my_io.synchronized {  
      my_io.eraseScreen  // 退出时的美化扫除
      my_io.homeCursor
    }
  }

  def connectionTimedOut(ce : ConnectionEvent) : Unit = {
    my_io.synchronized {
      my_io.write("Connection timed out.")
      my_io.flush
    }
    my_connection.close
    timer_task.cancel
  }

  def connectionIdle(ce : ConnectionEvent) : Unit = {
    // do nothing
  }

  // logout request是指ctrl-D
  def connectionLogoutRequest(ce : ConnectionEvent) : Unit = {
    my_io.synchronized {
      my_io.write("Connection logout request.")
      my_io.flush
    }
    my_connection.close
    timer_task.cancel
  }

  def connectionSentBreak(ce : ConnectionEvent) : Unit = {
    my_io.synchronized {
      my_io.write("User sent break.")
      my_io.flush
    }
    my_connection.close
    timer_task.cancel
  }

  def scheduled_refresh : Unit = {    
    if(my_io != null) my_io.synchronized { 
      try {
        if(!KestrelHandler.is_fine) {  // 显示一个醒目的警示
          my_io.eraseScreen
          my_io.setCursor(13, 29)
          my_io.write("KESTREL QUEUE IS DOWN")
          my_io.flush
        } else {  // 正常显示每个端口的状况
          my_io.eraseScreen
          my_io.setCursor(1, 1)
          my_io.write("MMS SENDER CONSOLE   " +   // 标题
                      (mode match {
                        case M_status => "STATUS"
                        case M_capacity => "INTERVAL OF SEND AND DAY CAPACITY"
                        case M_history => "SUCCESS RATE OF LAST TEN SENDINGS"
                      }))
          MmsDaemon.ports.map{ 
            port =>  // 为每个端口制作它的状态字符串
              "  " + port.toString + "  " + (
                mode match {
                  case M_status => port.get_status_string
                  case M_capacity => 
                    ((port.interval_of_send / 60 / 1000).toString + "min. " +
                     port.capacity.toString + "msg/day")
                  case M_history => port.success_rate_string  
                }
              )
          }.zipWithIndex.foreach{
            info => {  // 将状态字符串打印出来
              my_io.setCursor(info._2 / 2 + 2, (info._2 % 2)*40 + 1)
              my_io.write(info._1)
            }
          }        
          my_io.setCursor(caret / 2 + 2, (caret % 2)*40 + 1)
          my_io.write(">") // 显示光标位置
          my_io.flush
        }
      } catch {
        case e : java.net.SocketException => 
          // do nothing
      } 
    } 
  }
}


object TelnetShell {
  // 这个静态方法是telnetd开源库规定必须有的。
  def createShell : Shell = new TelnetShell
}

import java.util.{ Timer, TimerTask }

// 这个Timer一直存在。不过不同的shell会为它设置不同的timertask，而
// 在shell退出时，也只会将它自己的timertask给cancel掉。
object OurTimer {
  val timer = new Timer(true)

  def schedule(task : TimerTask, delay : Long, period : Long) : Unit = 
    timer.schedule(task, delay, period)
}

class TelnetRefreshTimerTask(shell : TelnetShell) extends TimerTask {
  def run : Unit = 
    try {
      shell.scheduled_refresh
    } catch { // 如果某个连接被异常中断，那么那个shell就来不及调用
              // cancel，所以在这里调用。因为一旦连接被异常中
              // 断，refresh对socket io的读写就会抛出异常。
      case e : java.net.SocketException => this.cancel
    }
}
