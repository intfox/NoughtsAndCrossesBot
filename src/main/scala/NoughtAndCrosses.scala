import cats.effect.concurrent.{Deferred, Ref, Semaphore}
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import cats.implicits._

import NoughtAndCrossesService.GameResult

trait NoughtAndCrossesService[F[_], GameInfo] {

  def startGame: F[GameInfo]

  def move(game: GameInfo): F[Move]

  def player(game: GameInfo): F[Player]

  def field(game: GameInfo): F[Seq[Seq[Option[Player]]]]

  sealed trait Move
  object Move {
    trait User extends Move {
      def apply(x: Int, y: Int): F[Unit]
    }
    trait Enemy extends Move {
      def apply(): F[Unit]
    }
    trait GameOver extends Move {
      def apply(): F[GameResult]
    }
  }
}

case class GameInfoImpl[F[_]](thatPlayer: Player, game: Ref[F, Game], playerSemaphore: Semaphore[F], enemySemaphore: Semaphore[F])

class NoughtAndCrossesServiceImpl[F[_] : Concurrent](ref: Ref[F, Option[Deferred[F, GameInfoImpl[F]]]]) extends NoughtAndCrossesService[F, GameInfoImpl[F]] {

  override def startGame: F[GameInfoImpl[F]] = for {
    myDeferred <- Deferred[F, GameInfoImpl[F]]
    enemyDeferredOption <- ref.modify{
      case Some(deferred) => (None, Some(deferred))
      case None => (Some(myDeferred), None)
    }
    gameInfo <- enemyDeferredOption match {
      case Some(enemyDeferred) => for {
        game <- Ref.of[F, Game](Game(3))
        thatPlayerSemaphore <- Semaphore[F](0)
        enemyPlayerSemaphore <- Semaphore[F](0)
        thatPlayer = Crosses
        _ <- enemyDeferred.complete(GameInfoImpl(thatPlayer.reverse, game, enemyPlayerSemaphore, thatPlayerSemaphore))
      } yield GameInfoImpl(thatPlayer, game, thatPlayerSemaphore, enemyPlayerSemaphore)
      case None => myDeferred.get
    }
  } yield gameInfo

  override def move(gameInfo: GameInfoImpl[F]): F[Move] = for {
    game <- gameInfo.game.get
    mv = game match {
      case game: Game.On => if(gameInfo.thatPlayer == game.thatMove) new Move.User {
        override def apply(x: Int, y: Int): F[Unit] = for {
          _ <- gameInfo.game.set(game.move(x, y))
          _ <- gameInfo.enemySemaphore.release
        } yield ()
      } else new Move.Enemy {
        override def apply(): F[Unit] = gameInfo.playerSemaphore.acquire
      }
      case game: Game.Over => new Move.GameOver {
        override def apply(): F[NoughtAndCrossesService.GameResult] = (game.winner.map {
          case gameInfo.thatPlayer => GameResult.Win
          case _ => GameResult.Lose
        } getOrElse GameResult.Draw).pure[F]
      }
    }
  } yield mv

  override def player(game: GameInfoImpl[F]): F[Player] = game.thatPlayer.pure[F]

  override def field(gameInfo: GameInfoImpl[F]): F[Seq[Seq[Option[Player]]]] = gameInfo.game.get.map( _.field )
}

object NoughtAndCrossesService {
  sealed trait GameResult
  object GameResult {
    object Win extends GameResult
    object Lose extends GameResult
    object Draw extends GameResult
  }

  def apply[F[_] : Concurrent]: F[NoughtAndCrossesService[F, GameInfoImpl[F]]] = Ref.of[F, Option[Deferred[F, GameInfoImpl[F]]]](None).map( new NoughtAndCrossesServiceImpl[F](_) )
}
