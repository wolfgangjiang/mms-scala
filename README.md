这是一个用于控制多个普通的短信收发modems（即俗称的“短信
猫”）发送彩信的服务器。它：

* 全部用scala语言写成

* 接受http post形式的api请求，根据api请求发送彩信

* 在jvm上实现了基于GPRS的PPP、IP、TCP和Smil协议栈

* 与kestrel队列结合，实现多台短信modem之间的自动负载均衡控制

* 有一个telnet控制界面

* 使用mongodb作为日志数据库

* 可长时间稳定工作
