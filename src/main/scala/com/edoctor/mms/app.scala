package com.edoctor.mms

import actors.ModemSession
import net.lag.configgy.Configgy


// temporarily disabled, because Wolfgang Jiang wants to launch from 
// another object App defined in another .scala file.
/*
object App extends Application{
  Configgy.configure("./mms.conf")
  new ModemSession(Parameters.mmscatIP, Parameters.mmscatPort).start
}

*/
