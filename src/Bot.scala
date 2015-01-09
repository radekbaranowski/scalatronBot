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


    val (opcode,paramMap) = CommandParser(input)

    opcode match {
      case "Welcome" =>

        var round = paramMap("round").toInt
        filePath = "d:\\botlog\\bot_"+round+".log"
        if (loggingEnabled) {logFile = new FileWriter(filePath)}
        "Status(text=hello)|Log(text="+filePath+")"

      case "React" =>

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

object CommandParser {
  def apply(command:String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if( segments.length!=2)
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0),segments(1))
    }

    val segments = command.split('(')
    if (segments.length != 2)
      throw new IllegalStateException("invalid command: " + command)

    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParam).toMap

    (segments(0),keyValuePairs)

  }


}
