package com.edoctor.mms

import org.apache.commons.httpclient.{ HttpClient, HttpStatus }
import org.apache.commons.httpclient.methods.{ GetMethod, PostMethod }
import org.apache.commons.httpclient.NameValuePair
import org.apache.commons.httpclient.methods.multipart.{ Part, StringPart, FilePart, MultipartRequestEntity }

object CosmsSPInfo {
  def send_mms_cosms(mobile : String, 
                     title : String, 
                     text : String, 
                     pic_url : String, 
                     mid_url : String,
                     temp_directory_path : String) : Boolean = {

    import java.io.File

    try {
      val line_end = "\n"
      val temp_path = new File(temp_directory_path)
      val ini_file = new File(temp_directory_path, "mms.ini")
      val txt_file = new File(temp_directory_path, "mms_text.txt")

      try {
        if(!temp_path.exists) temp_path.mkdirs
        if(!ini_file.exists) ini_file.createNewFile
        if(!ini_file.exists) txt_file.createNewFile
      } catch {
        case e : Exception => {
          Log.error("Cosms SP", e)
          return false
        }
      }

      val httpclient = new HttpClient()
      httpclient.getParams.setParameter("http.socket.timeout", 2000)
      httpclient.getParams.setParameter("http.connection.timeout", 2000)
      httpclient.getParams.setParameter("http.connection-manager.timeout", 2000L)

      // 不必同步，因为可能可能调用本函数的只有唯一的一个相关port线程，这样
      // 的线程不会同时存在2个以上。
      def file_write(file : File, content : String) : Unit = {
        var out_s : java.io.FileOutputStream = null
        try {
          out_s = new java.io.FileOutputStream(file)
          out_s.write(content.getBytes("gbk"))
          out_s.flush
        } finally {
          if(out_s != null)
            out_s.close
        }
      }
      
      def send_mms_post_to_cosms(mobile : String, 
                                 title : String, 
                                 ini_url : String) : Boolean = {
        val httpget = new GetMethod("http://www.cosms.cn/sms/putMt/")
        val query_pairs = Array(
          new NameValuePair("msgFormat", "2"),
          new NameValuePair("corpID", "2056842"),
          new NameValuePair("loginName", "3082"),
          new NameValuePair("password", "3082123"),
          new NameValuePair("mtLevel", "1"),
          new NameValuePair("Mobs", mobile),
          new NameValuePair("linkID", "3000"),
          new NameValuePair("mmsTitle", title),
          new NameValuePair("msg", ini_url))
        httpget.setQueryString(query_pairs)

        try {
          val status_code = httpclient.executeMethod(httpget)

          return (status_code == HttpStatus.SC_OK &&
                  httpget.getResponseBodyAsString.startsWith("100"))
        } catch {
          case e : Exception => {
            throw e
            return false
          }
        } finally {
          httpget.releaseConnection
        }
      }  

      def upload_file_to_cosms(file : File) : String = {
        val httppost = new PostMethod("http://www.cosms.cn/sms/FileUpload/")
        val parts : Array[Part] = Array(
          new StringPart("corpID", "2056842"),
          new StringPart("loginName", "3082"),
          new StringPart("password", "3082123"),
          new FilePart("F", file))
        httppost.setRequestEntity(
          new MultipartRequestEntity(parts, httppost.getParams))
        try {
          val status_code = httpclient.executeMethod(httppost)
          
          if(status_code == HttpStatus.SC_OK) 
            httppost.getResponseBodyAsString.split("\\s+")(1)
          else
            ""
        } catch {
          case e : Exception => {
            return ""
          }
        } finally {
          httppost.releaseConnection
        }
      }    

      def prepare_content(text : String, 
                          pic_url : String,
                          mid_url : String) : String = {
        file_write(txt_file, text)
        val uploaded_file_url = upload_file_to_cosms(txt_file)
        List(pic_url, uploaded_file_url, mid_url).filterNot(_.isEmpty).mkString(line_end)
      }                      

      val ini_file_content = prepare_content(text, pic_url, mid_url)
      file_write(ini_file, ini_file_content)

      val ini_url = upload_file_to_cosms(ini_file)

      if(ini_url.isEmpty) 
        return false
      else 
        send_mms_post_to_cosms(mobile, title, ini_url)
    } catch {
      case e : Exception => {
        Log.error("Cosms SP", e)
        return false
      }
    }
  }

  def send_mms_method(request : String, data : List[Byte]) : Boolean = {
    try {
      val type_example = Array("a")
      val fields : Array[String] = (new com.google.gson.Gson).fromJson(request, type_example.getClass)
      val date = fields(0)
      val target = fields(1)
      val subject = fields(2)
      val text = fields(3)
      val pic_url = fields(4)
      val mid_url : String = if(fields.length > 5) fields(5) else ""

      send_mms_cosms(target, subject, text, pic_url, mid_url, "./cosms_tmp/")
    } catch {
      case e : Exception => {
        Log.error("Cosms SP", e)
        return false
      }
    }
  }
  
  def example : Unit = {
    println(send_mms_cosms("13122747605", "新的sp彩信测试", "这是新的sp彩信测试之二",
                           "http://t.e-doctor.cn/system/mms/2011/1/12/1671.gif",
                           "http://in.edoctor.cn/caixin/mms1.mid", 
                           "."))
  }
}
