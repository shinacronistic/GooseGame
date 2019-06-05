package my.shina.games

import ImplicitTools._

import scala.util._

object GooseBoard {

  type BoardState = Map[Player,Int]

  private var players:BoardState = Map()

  def getPlayers:Set[Player] = players.keys.toSet

  val spaceList:Map[Int,Class[_ <: BoardSpace]] = {
    //per ora è bloccato così, ma nulla vieta di usare la reflection e vedere tutte le classi disponibili
    //per gli eventi legati ai tasselli, e generare la board secondo qualche criterio
    var temp = 1.to(63).map(a => a -> classOf[StandardSpace]).toMap[Int, Class[_ <: BoardSpace]]
      temp ++= Seq(5, 9, 14, 18, 23, 27).map(a => a -> classOf[TheGooseSpace]).toMap[Int, Class[_ <: BoardSpace]]
      temp += (0 -> classOf[StartSpace],63 -> classOf[VictorySpace],6 -> classOf[TheBridgeSpace])

    temp
  }


  def addPlayer(ps:Player) = {

    if (players.keys.toSet.contains(ps)) {

      s"$ps: already existing player".printLog
    }

    else {

      players+=(ps -> 0)

      playersToString(players.keys.toSet).printLog

    }

  }

  private var lastMoveBy:Option[Player] = None

  def prankOthers(player:Player,oldPosition:Int) = {
    val position=players.getOrElse(player,0)
    (players - player).find(_._2==position) match {
      case Some((p,s)) => s"On $position there is $p, who returns to ${oldPosition.getBoardSpaceName}".printLog;players+=(p->oldPosition)
      case None => print("")
    }


  }

  private def tryMovePlayer(player:Player,step1:Int,step2:Int):Try[Map[Player,Int]] = Try {

        player.isOnBoard

        val oldState = players.getOrElse(player, 0)

        val attempt = oldState + step1 + step2

        val ifAgain = lastMoveBy match {
          case Some(lp) => if (lp == player) "again and goes" else s"from ${oldState.getBoardSpaceName}"
          case None => s"from ${oldState.getBoardSpaceName}"
        }

        lastMoveBy = Some(player)

        players += (player -> attempt)

        s"""$player moves $ifAgain to ${attempt.getBoardSpaceName}.""".appendToLog

        prankOthers(player,oldState)

        attempt.getBoardSpace.endState(player,players).get

  }

  def movePlayer(player:Player,step1:Int,step2:Int):Unit = {
    tryMovePlayer(player,step1,step2) match {
      case Success(bs) => players=bs
      case Failure(e) => print("")/*println("No further movement")*/
    }
  }


  final val victorySpace=63

}