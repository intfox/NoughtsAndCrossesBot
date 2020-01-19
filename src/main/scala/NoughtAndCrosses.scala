trait NoughtAndCrossesService[F[_]] {
  import NoughtAndCrosses._
  type Game

  def startGame: F[Game]

  def move(game: Game): F[Move]

  sealed trait Move {
    type Field = Seq[Seq[Option[Player]]]
    def field: Field
  }
  object Move {
    trait User extends Move {
      def apply(x: Int, y: Int): F[Unit]
    }
    trait Enemy extends Move {
      def apply(): F[Unit]
    }
    trait GameOver extends Move {
      def apply(): F[Option[Player]]
    }
  }
}

object NoughtAndCrosses {
}
