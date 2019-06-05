package my.shina.games

import ImplicitTools._

import scala.util.Try

trait BoardSpace {

  def event(p:Player, pb:Map[Player,Int]):Any = "".printLog

  val spaceName:String

  final val endState: (Player, Map[Player,Int]) => Try[Map[Player,Int]] = (p, ps)
  => Try(event(p,ps).asInstanceOf[Map[Player,Int]])


}

class StartSpace(spaceNumber:Int) extends BoardSpace {

  override val spaceName: String = "Start"
}

class VictorySpace(spaceNumber:Int) extends BoardSpace {

  override def event(player:Player, pb:Map[Player,Int]) = s"$player Wins!!".printLog

  override val spaceName: String = spaceNumber.toString
}

class StandardSpace(spaceNumber:Int) extends BoardSpace {

  override val spaceName: String = spaceNumber.toString
}

class BounceSpace(spaceNumber:Int) extends BoardSpace {

  override def event(player:Player, pb:Map[Player,Int]):Map[Player,Int]= {

    val newAttempt = 2*GooseBoard.victorySpace - pb.getOrElse(player,0)

    s"$player bounces! $player returns to $newAttempt".printLog

    pb + (player->newAttempt)
  }

  override val spaceName: String = "63"
}

class TheBridgeSpace(spaceNumber:Int) extends BoardSpace {

  override def event(player:Player, pb:Map[Player,Int]) = {

    val newAttempt = pb.getOrElse(player,0) + 6

    s"$player jumps to $newAttempt.".printLog

    pb + (player->newAttempt)
  }

  override val spaceName: String = "The Bridge"
}

class TheGooseSpace(spaceNumber:Int) extends BoardSpace {

  override def event(player:Player, ps:Map[Player,Int]) = {

    player.moves(withRoll = false)

  }

  override val spaceName: String = s"$spaceNumber, The Goose"
}
