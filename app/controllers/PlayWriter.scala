package controllers

import scala.concurrent.Future


import scala.util.Random
import annotation.tailrec

import akka.actor.{Props, Actor}
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout

import scala.concurrent.duration._


//  copied from https://github.com/henrikengstrom/PlayMiniSample
//  http://letitcrash.com/post/17888436664/a-sample-application-showcasing-play-mini-and-akka

case class ShakespeareResult(shakespeareMagic: Set[String], unworthyWords: Set[String])

class ShakespeareActor extends Actor {

  import ShakespeareActor._

  lazy val randomGenerator = new Random
  implicit val timeout = Timeout(5000 milliseconds)

  import context.dispatcher

  def receive = {
    case actors: Int =>
      val futures = for (x <- 1 to actors) yield {
        context.actorOf(Props[MonkeyWorker]) ? randomGenerator.nextInt(100) mapTo manifest[Set[String]]
      }

      Future.sequence(futures) map {
        wordSets =>
          val mergedSet = wordSets reduce ((a, b) => a ++ b)
          val (shakespeare, unworthy) = mergedSet partition (x => Blueprint.contains(x))
          ShakespeareResult(shakespeare, unworthy)
      } pipeTo sender
  }
}


object ShakespeareActor {
  lazy val Blueprint = Set("to", "be", "or", "not")
}

class MonkeyWorker extends Actor {

  import WorkerActor._

  lazy val randomGenerator = new Random

  def receive = {
    // We simplify the word generation by saying that every word can be between 1 and 26 letters.
    // If in real life, i.e. when using monkeys, some statistical bias would be useful when handing out the
    // type writing instructions.
    case tries: Int =>
      var words = Set.empty[String]
      1 to tries foreach {
        x => words += generateWork(randomGenerator.nextInt(Letters.size))
      }
      sender ! words
  }

  def generateWork(letters: Int) = {
    @tailrec
    def trGeneration(letterNumber: Int, result: String): String = letterNumber match {
      case 0 => result.reverse
      case n => trGeneration(n - 1, result + Letters(randomGenerator.nextInt(Letters.size - 1)))
    }

    trGeneration(letters, "")
  }
}

object WorkerActor {
  lazy val Letters = ('a' to 'z').toSeq
}