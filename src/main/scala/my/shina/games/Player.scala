package my.shina.games

import scala.util.Random
import my.shina.games.ImplicitTools._

case class Player(name:String) {

  override def toString = name

  private val die=new Random()

  private var roll1:Int=6
  private var roll2:Int=6

  def addPlayer = GooseBoard.addPlayer(this)

  class PlayerNotAllowedException extends Exception

  def isOnBoard:Unit = GooseBoard.getPlayers.contains(this) match {
    case true => print("")
    case false => {s"""$this is not playing this game. add it with "add player <playerName>".""".printLog
      throw new PlayerNotAllowedException
    }
  }

  def rollIFNone(value:Option[Int]=None) = value match {
    case None => die.nextInt(5)+1
    case Some(a) => if(a<=6 && a>0) a else {println(s"a die rolled $a? really??");die.nextInt(5)+1}
  }

  def moves(step1:Option[Int]=None, step2:Option[Int]=None, withRoll:Boolean=true) = {

    if (withRoll) {
      roll1 = rollIFNone(step1)
      roll2 = rollIFNone(step2)
      s"$this rolls $roll1, $roll2.".appendToLog
    }

    GooseBoard.movePlayer(this,roll1,roll2)

  }

}
