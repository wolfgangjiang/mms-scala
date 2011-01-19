package com.edoctor.mms

import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.shell.Shell
import net.wimpi.telnetd.net.{ Connection, ConnectionEvent }
import net.wimpi.telnetd.io.BasicTerminalIO

object App {
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
    (new RearrangeTimedMessageRemindActor).start

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

class TelnetShell extends Shell {
  private object ShellMode extends Enumeration {
    type ShellMode = Value
    val M_status, M_capacity, M_history = Value
  }  
  import ShellMode._
  
  private var my_connection : Connection = null
  private var my_io : BasicTerminalIO = null
  private val timer_task = new TelnetRefreshTimerTask(this)
  private var mode : ShellMode = M_status
  private var caret = 0

  def run(conn : Connection) : Unit = {
    my_connection = conn
    my_io = conn.getTerminalIO

    OurTimer.schedule(timer_task, 0, 500)

    my_connection.addConnectionListener(this)

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
      val key = my_io.read
      if(key == LEFT && 
         caret % 2 == 1) caret -= 1
      else if(key == RIGHT && 
              caret % 2 == 0 
              && caret < MmsDaemon.ports.length - 1) caret += 1
      else if(key == UP &&
              caret >= 2) caret -= 2
      else if(key == DOWN &&
              caret < MmsDaemon.ports.length - 2) caret += 2
      else if(key == ENTER) mode match {
        case M_status => MmsDaemon.ports(caret).toggle_enabled
        case M_capacity => MmsDaemon.ports(caret).change_capacity
        case M_history => { }
      }
      else key.toChar match {
        case '1' => mode = M_status 
        case '2' => mode = M_capacity 
        case '3' => mode = M_history 
        case 'q' => going = false
        case 'Q' => going = false
        case _ => { }  // do nothing
      }
      scheduled_refresh
    }

    timer_task.cancel
    my_io.synchronized { 
      my_io.eraseScreen 
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
        if(!KestrelHandler.is_fine) {
          my_io.eraseScreen
          my_io.setCursor(13, 29)
          my_io.write("KESTREL QUEUE IS DOWN")
          my_io.flush
        } else {
          my_io.eraseScreen
          my_io.setCursor(1, 1)
          my_io.write("MMS SENDER CONSOLE   " + 
                      (mode match {
                        case M_status => "STATUS"
                        case M_capacity => "INTERVAL OF SEND AND DAY CAPACITY"
                        case M_history => "SUCCESS RATE OF LAST TEN SENDINGS"
                      }))
          MmsDaemon.ports.map{ 
            port =>
              "  " + port.ip_addr + " " + port.tcp_port + "  " + (
                mode match {
                  case M_status => port.get_status_string
                  case M_capacity => 
                    ((port.interval_of_send / 60 / 1000).toString + "min. " +
                     port.capacity.toString + "msg/day")
                  case M_history => port.success_rate_string  
                }
              )
          }.zipWithIndex.foreach{
            info => {
              my_io.setCursor(info._2 / 2 + 2, (info._2 % 2)*40 + 1)
              my_io.write(info._1)
            }
          }        
          my_io.setCursor(caret / 2 + 2, (caret % 2)*40 + 1)
          my_io.write(">")
          my_io.flush
        }
      } catch {
        case e : java.net.SocketException => 
          // do nothing
      } /*
      my_io.write("should refresh " + (mode match {
        case M_status => "status"
        case M_capacity => "capacity"
        case M_history => "history"
      }) + ".\n")
      my_io.flush */
    } 
  }
}


object TelnetShell {
  def createShell : Shell = new TelnetShell
}

import java.util.{ Timer, TimerTask }

object OurTimer {
  val timer = new Timer(true)

  def schedule(task : TimerTask, delay : Long, period : Long) : Unit = 
    timer.schedule(task, delay, period)
}

class TelnetRefreshTimerTask(shell : TelnetShell) extends TimerTask {
  def run : Unit = 
    try {
      shell.scheduled_refresh
    } catch {
      case e : java.net.SocketException => this.cancel
    }
}
