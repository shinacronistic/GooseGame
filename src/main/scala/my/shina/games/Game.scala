package my.shina.games

import my.shina.games.ImplicitTools._

object Game extends App {

  final val usage ="allowed commands are \n" +
    "add player <player name> \n" +
    "move <player name> optional(<step1>, <step2>)\n" +
    """"bye" to leave the Game"""

  def play(cmd:String):Unit= {

    val findRealSteps: List[String]=> List[Option[Int]]={
      steps => {
        val tentativeSteps = steps.mkString(" ").split("\\D+").filter(_.nonEmpty).map(_.toInt).map(Option(_)).toList
        (tentativeSteps ++ List[Option[Int]](None, None)).take(2)
      }

    }

    cmd.split(" ").toList match {
      case "bye" :: Nil => return
      case "add" :: "player" :: name :: Nil => name.addPlayer
      case "move" :: name :: steps => {
        val newSteps = findRealSteps(steps)

        name.moves(newSteps.head,newSteps.tail.head)
        }
      //case "prank mode" :: on_off ::Nil=> GooseBoard.setPrankMode()
      case _ => println(s"no action linked to such command \n$usage")
    }

    val ln = scala.io.Source.stdin.getLines.next().toString
    play(ln)
  }

  println("Welcome to My Goose Game, would you like to play?")

  play(scala.io.Source.stdin.getLines.next().toString)

  println("This My Goose Game is over! Thanks for playing with us. Bye Bye")

}