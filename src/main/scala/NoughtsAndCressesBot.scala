import canoe.api._
import canoe.models.{Chat, KeyboardButton, ReplyKeyboardMarkup, ReplyKeyboardRemove}
import canoe.models.messages.{TelegramMessage, TextMessage}
import canoe.syntax._
import cats.{Monad, MonadError}
import cats.effect.concurrent.{Deferred, MVar, Ref}
//import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import cats.effect._
import cats.syntax.functor._
//import cats.implicits._
import fs2.Stream
import HelperFunction._

import scala.language.higherKinds
//import scala.concurrent.ExecutionContext
//implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

sealed trait EndGame
case object Win extends EndGame
case object Lose extends EndGame

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

case class StartedGame(playerUser: Player, user: MVar[IO, (Int, Int)], enemy: MVar[IO, (Int, Int)], playerMove: Ref[IO, Player], filed: Ref[IO, Seq[Seq[Option[Player]]]])

private class NoughtsAndCrossesServiceImpl(private val searchQueue: MVar[IO, Deferred[IO, StartedGame]])(implicit val concurrent: Concurrent[IO]) extends NoughtsAndCrossesService[IO] {
  type Game = StartedGame

  override def startGame: IO[Game] =
    for {
      optionDeferredGame <- searchQueue.tryTake
      game <- optionDeferredGame match {
        case Some(value) =>
          for {
            randomBoolPlayerMove <- IO{new java.util.Random().nextBoolean()}
            user <- MVar.empty[IO, (Int, Int)]
            enemy <- MVar.empty[IO, (Int, Int)]
            playerMove <- Ref.of[IO, Player](if (randomBoolPlayerMove) Crosses else Noughts)
            randomBoolUserPlayerType <- IO{new java.util.Random().nextBoolean()}
            gameField <- Ref.of(Seq.fill(3)(Seq.fill(3)( None: Option[Player] )))
            game = StartedGame(if (randomBoolUserPlayerType) Crosses else Noughts, user, enemy, playerMove, gameField)
            _ <- value.complete(game)
          } yield game
        case None =>
          for {
            deferredGame <- Deferred[IO, Game]
            _ <- searchQueue.put(deferredGame)
            startedGame <- deferredGame.get
          } yield startedGame.copy(startedGame.playerUser.reverse, startedGame.enemy, startedGame.user)
      }
    } yield game

  private def checkCorrectCell(cell: (Int, Int), gameFiled: Seq[Seq[Option[Player]]]): Either[Throwable, (Int, Int)] =
    for {
      _ <- if(cell._1 < gameFiled.length && cell._1 >= 0 && cell._2 < gameFiled(0).length && cell._2 >= 0) Right(cell) else Left( new NoughtsAndCrossesService.ErrorIncorrectCell )
      _ <- if(gameFiled(cell._1)(cell._2).isEmpty) Right(cell) else Left(new NoughtsAndCrossesService.ErrorIncorrectCell)
    } yield cell

  private def checkWinGame(field: Seq[Seq[Option[Player]]] ): Option[Player] = {
    def check(player: Player): Boolean = {
      List(
        field(0).forall(_.contains(player)),
        field(1).forall(_.contains(player)),
        field(2).forall(_.contains(player)),
        field.map(_(0)).forall(_.contains(player)),
        field.map(_(1)).forall(_.contains(player)),
        field.map(_(2)).forall(_.contains(player)),
        field(0)(0).contains(player) && field(1)(1).contains(player) && field(2)(2).contains(player),
        field(0)(2).contains(player) && field(1)(1).contains(player) && field(2)(0).contains(player)).exists( identity )
    }
    if(check(Crosses)) Some(Crosses)
    else if(check(Noughts)) Some(Noughts)
    else None
  }

  override def move(game: Game): IO[Move] =
    for {
      playerMove <- game.playerMove.get
      move = playerMove match {
        case game.playerUser => new UserMove {
          override def apply(cell: (Int, Int)): IO[Either[EndGame, Unit]] =
            for{
              field <- game.filed.get
              cell <- IO.fromEither(checkCorrectCell(cell, field))
              _ <- game.playerMove.update( _.reverse )
              field <- game.filed.modify{ field =>
                val nextField = updatedMatrix(field)( cell._1, cell._2 )(Some(game.playerUser))
                (nextField, nextField)
              }
              res <- game.user.put(cell)
            } yield checkWinGame(field) match {
              case Some(game.playerUser) => Left(Win)
              case Some(_) => Left(Lose)
              case None => Right(res)
            }
        }
        case _ => new EnemyMove {
          override def apply(): IO[Either[EndGame, (Int, Int)]] =
            for {
              cell <- game.enemy.take
              field <- game.filed.get
            } yield checkWinGame(field) match {
              case Some(game.playerUser) => Left(Win)
              case Some(_) => Left(Lose)
              case None => Right(cell)
            }
        }
      }
    } yield move

//  override def move(game: Game): IO[Move] = ???
}

trait Print[F[_]]{
  def print( elem: String ): F[Unit]
}


object NoughtsAndCrossesBot extends IOApp {

  implicit val printIo = new Print[IO] {
    def print( elem: String ) = IO{println(elem)}
  }

  def run(args: List[String]): IO[ExitCode] =
    Stream
      .resource(TelegramClient.global[IO](BotToken.token))
      .flatMap { implicit client => Stream.eval(NoughtsAndCrossesService.apply).flatMap{ service =>
        Bot.polling[IO].follow(start(service))
      }}
      .compile.drain.as(ExitCode.Success)

  def start[F[_]: TelegramClient : Print ](noughtsAndCrossesService: NoughtsAndCrossesService[F])(implicit me: MonadError[F, Throwable]): Scenario[F, Unit] =
    for {
      chat <- Scenario.start(command("start").chat)
      _ <- Scenario.eval( chat.send( "Поиск противника" ) )
      gameN <- Scenario.eval(noughtsAndCrossesService.startGame)
      _ <- Scenario.eval( chat.send( "Протиник найден" , replyMarkup = Some(clearField)) )
      _ <- (for {
        end <- gameCicle(noughtsAndCrossesService, chat).apply(gameN)
        _ <- Scenario.eval( chat.send(s"Вы ${if(end == Win) "выйграли" else "проиграли"}!") )
      } yield ()).handleErrorWith{ err =>
        Scenario.eval( chat.send(s"Ошибка : ${err.getMessage}") ).flatMap( _ => Scenario.eval( implicitly[Print[F]].print( s" Error: ${err.getMessage} " ) ))
      }
    } yield ()

  private val coordinate = ((tmsg: TelegramMessage) => tmsg match {
    case msg: TextMessage => msg.text.split(" ").map( _.toIntOption ) match {
      case Array(Some(x), Some(y)) => Some(x, y)
      case _ => None
    }
    case _ => None
  }).unlift


  private val clearField: ReplyKeyboardMarkup = ReplyKeyboardMarkup((0 until 3).map(i => (0 until 3).map(j => KeyboardButton.text(s"$i $j") ) ))

  private class ErrorIncorrectInput extends Throwable("Incorrect input")

  private def strToCoordinate( str: String ): Either[ErrorIncorrectInput, (Int, Int)] = str.split(" ").map( _.toIntOption ) match {
    case Array(Some(x), Some(y)) => Right((x, y))
    case _ => Left(new ErrorIncorrectInput)
  }

  def gameCicle[F[_]: TelegramClient](noughtsAndCrossesService: NoughtsAndCrossesService[F], chat: Chat, field: ReplyKeyboardMarkup = clearField )(implicit me: MonadError[F, Throwable]): noughtsAndCrossesService.Game => Scenario[F, EndGame] =
    game => for {
      move <- Scenario.eval(noughtsAndCrossesService.move(game))
      move <- (move match {
        case userMove: noughtsAndCrossesService.UserMove =>
          for {
            _ <- Scenario.eval( chat.send("Ваш ход", replyMarkup = Some(field)) )
            userInputText <- Scenario.next(text)
            userInput <- Scenario.eval( me.fromEither(strToCoordinate(userInputText)) )
            end <- Scenario.eval(userMove.apply(userInput))
          } yield end.map( _ => field.copy(  keyboard = updatedMatrix(field.keyboard)(userInput._1, userInput._2)(KeyboardButton.text("X")) ))
        case enemyMove: noughtsAndCrossesService.EnemyMove =>
          for {
            move <- Scenario.eval(enemyMove.apply())
            _ <- move.map( move => Scenario.eval( chat.send(s"Ход противника ${move._1} ${move._2}", replyMarkup = Some(field)) ).void ).getOrElse( Scenario.done )
          } yield move.map{ case (i, j) => field.copy( keyboard = updatedMatrix(field.keyboard)(i, j)(KeyboardButton.text("O")) )}
      }).handleErrorWith{
        case _ : ErrorIncorrectInput => Scenario.pure( Right(field) )
        case _ : NoughtsAndCrossesService.ErrorIncorrectCell => Scenario.pure( Right(field) )
        case err => Scenario.eval( me.raiseError(err) )
      }
      end <- move match {
        case Right(value) => gameCicle(noughtsAndCrossesService, chat, value).apply(game)
        case Left(end) => Scenario.pure[F, EndGame](end)
      }
   } yield end

}

object HelperFunction {
  def updatedMatrix[T](matrix: Seq[Seq[T]])(index1: Int, index2: Int)(elem: T): Seq[Seq[T]] = matrix.updated(index1, matrix(index1).updated(index2, elem))
}