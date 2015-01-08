package scalatron.botwar.botPlugin.baranBot

import java.io.FileWriter

class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {
  var n = 0
  var filePath:String = _
  var logFile:FileWriter = _
  val loggingEnabled = false

  def respond(input: String) = {
    n += 1
    val tokens = input.split('(')
    val opcode = tokens(0)

    opcode match {
      case "Welcome" =>
        val rest = tokens(1).dropRight(1)
        val params = rest.split(',')
        val strPairs = params.map(s => s.split('='))
        val kvPairs = strPairs.map(a => (a(0), a(1)))
        val paramMap = kvPairs.toMap
        var round = paramMap("round").toInt
        filePath = "d:\\botlog\\bot_"+round+".log"
        if (loggingEnabled) {logFile = new FileWriter(filePath)}
        "Status(text=hello)|Log(text="+filePath+")"

      case "React" =>
        val rest = tokens(1).dropRight(1)
        val params = rest.split(',')
        val strPairs = params.map(s => s.split('='))
        val kvPairs = strPairs.map(a => (a(0), a(1)))
        val paramMap = kvPairs.toMap
        val energy = paramMap("energy").toInt
        if (loggingEnabled) {
          logFile.append(paramMap("time").toCharArray)
          logFile.append('\n')
          logFile.append(paramMap("view").toString)
          logFile.append('\n')
        }

        "Status(text=Round:" + filePath + ")"

      case "Goodbye" =>
        if (loggingEnabled) {logFile.close()}

    }

  }
}


