package com.edoctor.mms

// 这里对apache httpclient的使用都是按照3.1版本来使用的，其它的版本有不
// 同的用法。
import org.apache.commons.httpclient.{ HttpClient, HttpStatus }
import org.apache.commons.httpclient.methods.{ GetMethod, PostMethod }
import org.apache.commons.httpclient.NameValuePair
import org.apache.commons.httpclient.methods.multipart.{ Part, StringPart, FilePart, MultipartRequestEntity }

object CosmsSPInfo {
  // cosms特服号提供商的API主要包装在本函数中。本函数包含若干子函数。
  def send_mms_cosms(mobile : String, 
                     title : String, 
                     text : String, 
                     pic_url : String, 
                     mid_url : String,
                     temp_directory_path : String) : Boolean = {

    import java.io.File

    // 这个try用来兜住任何异常，将异常捕获后，转化为false返回值来报告失败
    try { 
      val line_end = "\n"
      val temp_path = new File(temp_directory_path)
      val ini_file = new File(temp_directory_path, "mms.ini")
      val txt_file = new File(temp_directory_path, "mms_text.txt")

      // 临时文件ini_file用来表示彩信的模板内容，里面是要显示的东西的
      // url。而彩信中的文本也必须用url来表示，所以要先用临时文件
      // txt_file保存，然后将txt_file上传到cosms的服务器上。这是cosms
      // API所规定的流程。

      try {
        if(!temp_path.exists) temp_path.mkdirs
        if(!ini_file.exists) ini_file.createNewFile
        if(!ini_file.exists) txt_file.createNewFile
      } catch { 
        // 如果建立临时文件失败，则整个彩信发送失败。这种失败很可能是因
        // 为没有写权限而造成的。
        case e : Exception => {
          Log.error("Cosms SP", e)
          return false
        }
      }

      val httpclient = new HttpClient()
      // 必须进行timeout的显式设置，不然的话，apache的httpclient对于反
      // 应慢的网站可能会长时间地等待下去。在运行本函数的时候，主循环是
      // 阻塞的！所以本函数必须快速运行。
      httpclient.getParams.setParameter("http.socket.timeout", 2000)
      httpclient.getParams.setParameter("http.connection.timeout", 2000)
      httpclient.getParams.setParameter("http.connection-manager.timeout", 2000L)

      // 将临时文件写入硬盘。不必刻意用文件锁来同步，因为可能调用本函数
      // 的只有唯一的一个相关port线程，或者干脆就是在主线程阻塞。这样的
      // 线程不会同时存在2个以上。而其它的SPPort也不会写入与我们相同的
      // 临时文件。
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

      // 对cosms特服号提供商的服务器发起http请求。
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

          // cosms规定，response body以数字开头，风格类似于http标准的
          // status码。其中100表示成功，其它表示各种失败。这些失败的代
          // 码也不妨在以后添加“记录到log”的代码，以供大量失败时调出来
          // 进行分析。
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

      // 这就是将临时文件上传到cosms服务器的代码。返回的是该临时文件上
      // 传后获得的url。如果上传失败，就返回空字符串""。这个空字符串可
      // 以安全地写入ini文件中。不过也许以后改成Option[String]更优雅一
      // 些？
      def upload_file_to_cosms(file : File) : String = {
        // 使用PostMethod和MultipartRequestEntity两个对象来表示http文件
        // 上传，是apache httpclient 3.1推荐的正确方法。不同的版本有不
        // 同的方法。
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
          
          // 这里，responsebody如果不是表示上传成功，则可能就没有第二
          // 项，也就是说，取用数组元素(1)会导致越界异常，越界异常就在
          // 下面接住。
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

      // 本函数纯粹用来写入ini文件的内容。它很简单，就只是三个url各占一
      // 行而已。如果某个url是空字符串，那么就会被过滤掉。不过如果是
      // null，现在的版本恐怕会写为"null"这四个字符，以后应该改掉这一点。
      def prepare_content(text : String, 
                          pic_url : String,
                          mid_url : String) : String = {
        file_write(txt_file, text)
        val uploaded_file_url = upload_file_to_cosms(txt_file)
        List(pic_url, uploaded_file_url, mid_url).filterNot(_.isEmpty).mkString(line_end)
      }                      

      // 准备好ini文件，写入硬盘。
      val ini_file_content = prepare_content(text, pic_url, mid_url)
      file_write(ini_file, ini_file_content)

      // 将ini文件也上传。
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
    try { // 这里对request的解析与MmsDaemon中重复了，以后应当想办法合并。
      val type_example = Array("a")
      val fields : Array[String] = (new com.google.gson.Gson).fromJson(request, type_example.getClass)
      val date = fields(0)
      val target = fields(1)
      val subject = fields(2)
      val text = fields(3)
      val pic_url = fields(4)
      val mid_url : String = if(fields.length > 5) fields(5) else ""

      // 临时目录默认为"cosms_tmp"，这是写死在代码里的。可能以后也没有
      // 必要将它变成可配置的，因为毕竟整个cosms特服号的逻辑都是写死在
      // 代码里的。
      send_mms_cosms(target, subject, text, pic_url, mid_url, "./cosms_tmp/")
    } catch {
      case e : Exception => {
        Log.error("Cosms SP", e)
        return false
      }
    }
  }
  
  // 这个方法并不会被实际上线运行的daemon所调用，仅仅是作为一个文档留存
  // 在这里，表示这个sp的使用方法。
  def example : Unit = {
    println(send_mms_cosms("13122747605", "新的sp彩信测试", "这是新的sp彩信测试之二",
                           "http://t.e-doctor.cn/system/mms/2011/1/12/1671.gif",
                           "http://in.edoctor.cn/caixin/mms1.mid", 
                           "."))
  }
}
