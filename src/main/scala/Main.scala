import NoughtAndCrossesService.GameResult
import cats.effect.{ExitCode, IO, IOApp}
import canoe.api._
import canoe.models.Chat
import canoe.models.messages.TextMessage
import canoe.syntax._
import fs2.Stream

object Main extends IOApp{
  override def run(args: List[String]): IO[ExitCode] = for {
    service <- NoughtAndCrossesService[IO]
    _ <- Stream
      .resource(TelegramClient.global[IO](BotToken.token))
      .flatMap { implicit client => Bot.polling[IO].follow(noughtAndCrossesBot(service)) }
      .compile.drain
  } yield ExitCode.Success

  def noughtAndCrossesBot[F[_] : TelegramClient, Game](noughtAndCrossesService: NoughtAndCrossesService[F, Game]): Scenario[F, Unit] = for {
    m <- Scenario.expect(command("start"))
    game <- Scenario.eval(noughtAndCrossesService.startGame)
    messageGame <- Scenario.eval(m.chat.send("Игра:"))
    result <- gameLoop(noughtAndCrossesService, messageGame, game)
    messageResult = result match {
      case GameResult.Win => "Вы победили!"
      case GameResult.Lose => "Вы проиграли."
      case GameResult.Draw => "Ничья."
    }
    _ <- Scenario.eval(m.chat.send(messageResult))
  } yield ()

  def gameLoop[F[_] : TelegramClient, Game](noughtAndCrossesService: NoughtAndCrossesService[F, Game], messageButtons: TextMessage, game: Game): Scenario[F, GameResult] = for {
    field <- Scenario.eval(noughtAndCrossesService.field(game))
    _ <- updateButton(field, messageButtons)
    step <- Scenario.eval(noughtAndCrossesService.move(game))
    _ <- step match {
      case move: noughtAndCrossesService.Move.User => for {
        _ <- Scenario.expect(any)
        _ <- Scenario.eval(move(1, 2))
      } yield ()
    }
  } yield ()

  def updateButton[F[_]: TelegramClient, Game](field: Seq[Seq[Option[Player]]], messageButton: TextMessage): Scenario[F, Unit] = for {
    _ <-
  } yield ()
}
