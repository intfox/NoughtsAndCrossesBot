import canoe.api._
import canoe.models.Chat
import canoe.models.messages.{TelegramMessage, TextMessage}
import canoe.syntax._
import cats.Monad
import cats.effect.concurrent.{Deferred, MVar, Ref}
//import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import cats.effect._
import cats.syntax.functor._
//import cats.implicits._
import fs2.Stream

import scala.language.higherKinds
//import scala.concurrent.ExecutionContext
//implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

sealed trait EndGame
case object Win extends EndGame
case object Lose extends EndGame

sealed trait Player extends Product {
  def reverse: Player = this match {
    case Crosses => Noughts
    case Noughts => Crosses
  }
}
case object Crosses extends Player// Крестики
case object Noughts extends Player// Нолики

trait NoughtsAndCrossesService[F[_]] {
  type Game

  sealed trait Move
  trait UserMove extends Move {
    def apply(cell: (Int, Int)): F[Either[EndGame, Unit]]
  }
  trait EnemyMove extends Move {
    def apply(): F[Either[EndGame, (Int, Int)]]
  }

  def startGame: F[Game]
  def move(game: Game): F[Move]
}

object NoughtsAndCrossesService {
  def apply(implicit concurrent: Concurrent[IO]): IO[NoughtsAndCrossesService[IO]] =
    for {
      searchQueue <- MVar.empty[IO, Deferred[IO, StartedGame]]
    } yield new NoughtsAndCrossesServiceImpl(searchQueue)

  class ErrorIncorrectCell extends Throwable("Incorrect Cell")
}

case class StartedGame(playerUser: Player, user: MVar[IO, (Int, Int)], enemy: MVar[IO, (Int, Int)], playerMove: Ref[IO, Player], filed: Ref[IO, Array[Array[Option[Player]]]])

private class NoughtsAndCrossesServiceImpl(private val searchQueue: MVar[IO, Deferred[IO, StartedGame]])(implicit val concurrent: Concurrent[IO]) extends NoughtsAndCrossesService[IO] {
  type Game = StartedGame

  override def startGame: IO[Game] =
    for {
      optionDeferredGame <- searchQueue.tryTake
      game <- optionDeferredGame match {
        case Some(value) =>
          IO{ println("Противник найден.") }.flatMap{ _ => IO{new java.util.Random().nextBoolean()}.flatMap{ randomBoolPlayerMove =>
            MVar.empty[IO, (Int, Int)].flatMap{ user =>
              MVar.empty[IO, (Int, Int)].flatMap{ enemy =>
                Ref.of[IO, Player](if (randomBoolPlayerMove) Crosses else Noughts).flatMap{ playerMove =>
                  IO{new java.util.Random().nextBoolean()}.flatMap{ randomBoolUserPlayerType =>
                    Ref.of(Array.fill(3)(Array.fill(3)( None: Option[Player] ))).flatMap{ gameField =>
                      val game = StartedGame(if (randomBoolUserPlayerType) Crosses else Noughts, user, enemy, playerMove, gameField)
                      value.complete(game).map( _ => game)
                    }
                  }
                }
              }
            }
          }}
        case None =>
          IO{println("Противник не найден")}.flatMap{ _ => Deferred[IO, Game].flatMap { deferredGame =>
            searchQueue.put(deferredGame).flatMap(_ => deferredGame.get).map{ startedGame =>
              startedGame.copy(startedGame.playerUser.reverse, startedGame.enemy, startedGame.user)
            }
          }}
      }
    } yield game

  private def checkCorrectCell(cell: (Int, Int), gameFiled: Array[Array[Option[Player]]]): Either[Throwable, (Int, Int)] =
    for {
      _ <- if(cell._1 < gameFiled.length && cell._1 >= 0 && cell._2 < gameFiled(0).length && cell._2 >= 0) Right(cell) else Left( new NoughtsAndCrossesService.ErrorIncorrectCell )
      _ <- if(gameFiled(cell._1)(cell._2).isEmpty) Right(cell) else Left(new NoughtsAndCrossesService.ErrorIncorrectCell)
    } yield cell

  override def move(game: Game): IO[Move] =
    for {
      playerMove <- game.playerMove.get
      move = playerMove match {
        case game.playerUser => new UserMove {
          override def apply(cell: (Int, Int)): IO[Either[EndGame, Unit]] = game.filed.get.flatMap{ field =>
            IO.fromEither(checkCorrectCell(cell, field)).flatMap{ cell =>
              game.playerMove.update( _.reverse ).flatMap{ _ =>
                game.user.put(cell).map( Right(_) )
              }
            }
          }
        }
        case _ => new EnemyMove {
          override def apply(): IO[Either[EndGame, (Int, Int)]] = game.enemy.take.map( Right(_) )
        }
      }
    } yield move

//  override def move(game: Game): IO[Move] = ???
}

object NoughtsAndCressesBot extends IOApp {


  def run(args: List[String]): IO[ExitCode] =
    Stream
      .resource(TelegramClient.global[IO](BotToken.token))
      .flatMap { implicit client => Bot.polling[IO].follow(start(???)) }
      .compile.drain.as(ExitCode.Success)

  def start[F[_]: TelegramClient](noughtsAndCrossesService: NoughtsAndCrossesService[F]): Scenario[F, Unit] =
    for {
      chat <- Scenario.start(command("start").chat)
      gameN <- Scenario.eval(noughtsAndCrossesService.startGame)
      end <- gameCicle(noughtsAndCrossesService, chat).apply(gameN)
      _ <- Scenario.eval( chat.send(s"You ${if(end == Win) "win" else "lose"}!") )
    } yield ()

  private val coordinate = ((tmsg: TelegramMessage) => tmsg match {
    case msg: TextMessage => msg.text.split(" ").map( _.toIntOption ) match {
      case Array(Some(x), Some(y)) => Some(x, y)
      case _ => None
    }
    case _ => None
  }).unlift

  def gameCicle[F[_]: TelegramClient](noughtsAndCrossesService: NoughtsAndCrossesService[F], chat: Chat): noughtsAndCrossesService.Game => Scenario[F, EndGame] =
    game => for {
      move <- Scenario.eval(noughtsAndCrossesService.move(game))
      move <- move match {
        case userMove: noughtsAndCrossesService.UserMove =>
          for {
            userInput <- Scenario.next(coordinate)
            end <- Scenario.eval(userMove.apply(userInput))
          } yield end
        case enemyMove: noughtsAndCrossesService.EnemyMove =>
          for {
            move <- Scenario.eval(enemyMove.apply())
            _ <- move.map( move => Scenario.eval( chat.send(s"Ход противника ${move._1} ${move._2}") ).void ).getOrElse( Scenario.done )
            end = move.map( _ => ())
          } yield end
      }
      end <- move.swap.map(Scenario.pure[F, EndGame]).getOrElse( gameCicle( noughtsAndCrossesService, chat ).apply(game) )
    } yield end

}
