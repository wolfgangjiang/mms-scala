package com.edoctor.mms

import com.mongodb.Mongo
import com.mongodb.BasicDBObject

object Log {
  private val date_formatter = 
    new java.text.SimpleDateFormat("yyyy.MM.dd-kk:mm")

  private def time_string(time_millis : Long) : String = 
    date_formatter.format(new java.util.Date(time_millis))  

  private val mongo_coll = 
    new Mongo("127.0.0.1").getDB("mmslog").getCollection("log" + time_string(System.currentTimeMillis))

  private def write(log_data : BasicDBObject) : Unit = {
    log_data.put("time", time_string(System.currentTimeMillis))
    mongo_coll.insert(log_data)
  }

  private def write(level : String,
                    module_name : String, 
                    message : String) : Unit = {
    val log_data = new BasicDBObject()
    log_data.put("level", level)
    log_data.put("module", module_name)
    log_data.put("message", message)
    write(log_data)    
  }                    

  def info(module_name : String, message : String) : Unit = {
    write("info", module_name, message)
    println(module_name + " : " + message)
  }

  def debug(module_name : String, message : String) : Unit = 
    write("debug", module_name, message)
  
  def error(module_name : String, message : String) : Unit = {
    write("error", module_name, message)
    println("ERROR " + module_name + " : " + message)
  }

  def error(module_name : String, e : Exception) : Unit = {
    val sw = new java.io.StringWriter()
    e.printStackTrace(new java.io.PrintWriter(sw))
    error(module_name, sw.toString)
  }

  def bytes(module_name : String, message: String) : Unit = 
    write("bytes", module_name, message)
}
