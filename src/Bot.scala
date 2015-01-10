package scalatron.botwar.botPlugin.baranBot

import java.io.FileWriter

import scala.util.Random

class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {

  var n = 0
  var filePath: String = _
  var logFile: FileWriter = _
  val loggingEnabled = false
  var output = ""
  val rnd = new Random()
  var dirLastChanged = 0
  var lastSpawn = 0
  var currDx = 1
  var currDy = 1

  def appendCommand(s: String) = { output += (if(output.isEmpty) s else "|" + s) }

  def respond(input: String) = {
    output=""
    val (opcode, paramMap) = CommandParser(input)

    def detectObstacle(view: String) ={
      val leftSide = List(480)
      val rightSide = List(482)
      val upSide = List(419)
      val downSide = List(512 )

      for(i <- downSide) {
         view(i) match {
           case 'W' => currDy = -1
             appendCommand("Status(text=obstacle down)")
           case _ => currDy
         }
       }

      for(i <- upSide) {
        view(i) match {
          case 'W' => currDy = 1
            appendCommand("Status(text=obstacle up)")
          case _ => currDy
        }
      }

      for (i <- leftSide){
        view(i) match {
          case 'W' => currDx = 1
            appendCommand("Status(text=obstacle left)")
          case _ => currDx
        }
      }

      for (i <- rightSide){
        view(i) match {
          case 'W' => currDx = -1
            appendCommand("Status(text=obstacle right)")
          case _ => currDx
        }
      }
    }


    def detectStuff(view: String):Int ={
      /*val leftSide = List(478,479,480)
      val rightSide = List(482,483,484)
      val upSide = List(388,419,450)
      val downSide = List(512,543,574)
      val NE = List(391,421,451)
      val NW = List(385,417,449)
      val SE = List(513,545,577)
      val SW = List(511,541,571)*/

      val leftSide = List(479,479)
      val rightSide = List(481,482)
      val upSide = List(418,449)
      val downSide = List(511,542)
      val NE = List(420,450)
      val NW = List(416,448)
      val SE = List(512,544)
      val SW = List(510,540)


      for(i <- downSide) {
        view(i) match {
          case 'P' => currDy = 1;currDx=0
            appendCommand("Status(text=stuff down"+ view(478)+view(479)+view(480)+ ")");dirLastChanged=timestamp()
          case 'B' => currDy = 1;currDx=0
            appendCommand("Status(text=stuff down"+ view(478)+view(479)+view(480)+ ")");dirLastChanged=timestamp()
          case _ =>  0
        }
      }

      for(i <- upSide) {
        view(i) match {
          case 'P' => currDy = -1;currDx=0
            appendCommand("Status(text=stuff up)");dirLastChanged=timestamp()
          case 'B' => currDy = -1;currDx=0
            appendCommand("Status(text=stuff up)");dirLastChanged=timestamp()
          case _ =>  0

        }
      }

      for (i <- leftSide){
        view(i) match {
          case 'P' => currDy = 0;currDx= -1
            appendCommand("Status(text=stuff left)");dirLastChanged=timestamp()
          case 'B' => currDy = 0;currDx= -1
            appendCommand("Status(text=stuff left)");dirLastChanged=timestamp()
          case _ =>  0

        }
      }

      for (i <- rightSide){
        view(i) match {
          case 'P' => currDy = 0;currDx=1
            appendCommand("Status(text=stuff right)");dirLastChanged=timestamp()
          case 'B' => currDy = 0;currDx=1
            appendCommand("Status(text=stuff right)");dirLastChanged=timestamp()
          case _ =>  0

        }
      }

      for(i <- NE) {
        view(i) match {
          case 'P' => currDy = -1;currDx=1
            appendCommand("Status(text=stuff NE)");dirLastChanged=timestamp()
          case 'B' => currDy = -1;currDx=1
            appendCommand("Status(text=stuff NE)");dirLastChanged=timestamp()
          case _ =>  0

        }
      }

      for(i <- NW) {
        view(i) match {
          case 'P' => currDy = -1;currDx= -1
            appendCommand("Status(text=stuff NW)");dirLastChanged=timestamp()
          case 'B' => currDy = -1;currDx= -1
            appendCommand("Status(text=stuff NW)");dirLastChanged=timestamp()
          case _ =>  0

        }
      }

      for(i <- SW) {
        view(i) match {
          case 'P' => currDy = 1;currDx= -1
            appendCommand("Status(text=stuff SW)");dirLastChanged=timestamp()
          case 'B' => currDy = 1;currDx= -1
            appendCommand("Status(text=stuff SW)");dirLastChanged=timestamp()
          case _ =>  0

        }
      }

      for(i <- SE) {
        view(i) match {
          case 'P' => currDy = 1;currDx= 1
            appendCommand("Status(text=stuff SE)");dirLastChanged=timestamp()
          case 'B' => currDy = 1;currDx= 1
            appendCommand("Status(text=stuff SE)");dirLastChanged=timestamp()
          case _ =>  0

        }
      }

return 0
    }

    def timestamp() = {
      paramMap("time").toInt
    }
      opcode match {
        case "Welcome" =>

          var round = paramMap("round").toInt
          filePath = "d:\\botlog\\bot_" + round + ".log"
          if (loggingEnabled) {
            logFile = new FileWriter(filePath)
          }
          "Status(text=hello)|Log(text=" + filePath + ")"

        case "React" =>

          if (paramMap("generation").toInt == 0) {
            val energy = paramMap("energy").toInt



            if (loggingEnabled) {
              logFile.append(paramMap("time").toCharArray)
              logFile.append('\n')
              logFile.append(paramMap("view").toString)
              logFile.append('\n')
            }

            if (timestamp() == 2) {
              currDx = rnd.nextInt(3) - 1
              currDy = rnd.nextInt(3) - 1
              appendCommand("Move(direction=" + currDx + ":" + currDy + ")")
            }


            if(timestamp() - dirLastChanged > 25) {
              dirLastChanged = timestamp()
              currDx = rnd.nextInt(3) - 1
              currDy = rnd.nextInt(3) - 1
            }

            if( timestamp() - lastSpawn  > 15) {
              lastSpawn = timestamp()
            appendCommand("Spawn(direction=-" + currDx + ":-" + currDy + ",energy=100,bornon="+paramMap("time")+",initX="+currDx+ "initY="+ currDy+")")
            }

            //detectObstacle(paramMap("view"))
            detectStuff(paramMap("view"))
            appendCommand("Move(direction=" + currDx + ":" + currDy + ")")
          }

          if (paramMap("generation").toInt != 0) {

            if (timestamp() - paramMap("bornon").toInt < 30 ) {

              output = "Status(text=im a bot born on " + paramMap("bornon") + ")|Move(direction="+currDx+":"+currDy+")"
            } else {

              output = "Say(text=exploding)|Explode(size=5)"
            }


          }

        output
        case "Goodbye" =>
          if (loggingEnabled == true) {
            logFile.close()
            appendCommand("Status(text=goodbye")
          }


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
