package controllers

import play.api.mvc._

import play.api.mvc.Action


import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import play.libs.Akka

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


  lazy val shakespeare = Akka.system().actorOf(Props[ShakespeareActor], "shakespeare")
  implicit val timeout = Timeout(5000 milliseconds)

  def writeShakespeare(numberMonkeys:Int) = Action {

    val start = System.nanoTime

    Async {
      import play.api.libs.concurrent.Execution.Implicits._

      (shakespeare ? numberMonkeys).mapTo[ShakespeareResult].map {
        shakespeareResult  =>
        // We have a result - make some fancy pantsy presentation of it
          val builder = new StringBuilder
          builder.append("SHAKESPEARE WORDS:\n")
          shakespeareResult.shakespeareMagic.foreach { w => builder.append(w + "\n") }
          builder.append("UNWORTHY WORDS CREATED: " + shakespeareResult.unworthyWords.size + "\n")
          builder.append("In " + (System.nanoTime - start) / 1000 + "us\n")
          Ok(builder.toString)
      }
    }
  }

}
