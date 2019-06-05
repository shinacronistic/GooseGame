package my.shina.games

object ImplicitTools {

  type Players = Set[Player]

  def playersToString(players:Players):String = s"""Players are ${players.mkString(", ")}"""

  implicit class PlayerImpl(name:String) extends Player(name:String)

  implicit class BoardSpaceImpl(position:Int) {

    private val spaceClassName = GooseBoard.spaceList.getOrElse(position,classOf[BounceSpace]).toString.split(" ")(1)


    val getBoardSpace:BoardSpace = Class.forName(spaceClassName).getConstructor(classOf[Int])
      .newInstance(position.asInstanceOf[Integer]).asInstanceOf[BoardSpace]

    def getBoardSpaceName = {

      getBoardSpace.spaceName
    }

  }

  implicit class GameLog(message:String ="") {

    def appendToLog = {
      GameLog.append(message)
    }

    def printLog= {
      GameLog.appendAndPrint(message)
    }

    def getMessage= message

    def toList = List(this)

    def asGamelog = this

  }

  object GameLog {

    private var logLines:List[GameLog] = List[GameLog]()

    def appendAndPrint(message:String) = {
      append(message)
      val lines = logLines.map(l => l.getMessage)
      println(lines.mkString(" "))
      logLines = List[GameLog]()

    }

    def append(message:String) = {
      logLines = logLines ++ message.asGamelog.toList
    }

    def asList() = logLines
  }

}
