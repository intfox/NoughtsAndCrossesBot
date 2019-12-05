import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.funsuite.AnyFunSuite

class TestStartGame extends AnyFunSuite {

  import cats.implicits._
  import scala.concurrent.ExecutionContext
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  import scala.concurrent.duration._
  val timer = IO.timer(ExecutionContext.global)

  test("test start game") {
    (for {
      service <- NoughtsAndCrossesService.apply
      a <- (timer.sleep(3.seconds).flatMap(_ => service.startGame), service.startGame).parMapN( (game1, game2) => (game1, game2) )
    } yield a).unsafeRunTimed(5.seconds) match {
      case Some((game1, game2)) => assert(game1 != game2)
      case None => assert(false)
    }
  }

  test("ping pong test move") {
    def pingPond(service: NoughtsAndCrossesService[IO], cell: (Int, Int)): service.Game => IO[Unit] = game =>
      for {
        move <- service.move(game)
        _ <- move match {
          case userMove: service.UserMove => IO{ println(s"Send cell: $cell") }.flatMap( _ => userMove.apply(cell)).map( _ => () )
          case enemyMove: service.EnemyMove => enemyMove.apply().flatMap( enemyCell => IO{ println(s"Accept cell: $enemyCell") } )
        }
      } yield ()

    (for {
      service <- NoughtsAndCrossesService.apply
      a <- (timer.sleep(1.seconds).flatMap(_ => service.startGame), service.startGame).parMapN( (game1, game2) => (game1, game2) )
      (game1, game2) = a
      _ <- (pingPond(service, (1, 1))(game1), pingPond(service, (2, 2))(game2)).parMapN( (_, _) => () )
      _ <- (pingPond(service, (1, 1))(game1), pingPond(service, (2, 2))(game2)).parMapN( (_, _) => () )
    } yield ()).unsafeRunSync()
    assert(true)
  }
}
