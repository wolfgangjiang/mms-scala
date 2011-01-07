package com.edoctor.mms

import actors.ModemSession
import net.lag.configgy.Configgy

object App extends Application{
  Configgy.configure("./mms.conf")
  new ModemSession(Parameters.mmscatIP, Parameters.mmscatPort).start
}