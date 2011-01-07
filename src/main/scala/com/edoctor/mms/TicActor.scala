package com.edoctor.mms

import scala.actors.Actor

/**
 * Created by IntelliJ IDEA.
 * User: troya
 * Date: Dec 31, 2010
 * Time: 12:52:18 PM
 * To change this template use File | Settings | File Templates.
 */

object TicActor extends Actor {
  def act() : Unit = {
    println("TicActor started")
    while(true) {
      TicManagerActor ! 'tic
      Thread.sleep(1)
    }
  }
}

class Tic(client : Actor) extends Actor {
  def act() : Unit = {
    loop {
      client ! 'tic
      Thread.sleep(1)
    }
  }
}

object TicManagerActor extends Actor {
  var actorPool = Set[Actor]()

  def act() : Unit = {
    loop {
      receive {
        case 'tic =>
          actorPool.foreach(_ ! 'tic)
        case ('register, actor : Actor) =>
          actorPool = actorPool + actor
        case ('unregister, actor : Actor) =>
          actorPool = actorPool - actor
        case 'stop =>
          return
      }
    }
  }
}

class TestActor(id : Int) extends Actor {
  def act() : Unit = {
    receive {
      case 'tic => println(id) ; act()
      case 'exit => println("exit" + id) // exit
    }
  }

  override def toString : String = id.toString
}
