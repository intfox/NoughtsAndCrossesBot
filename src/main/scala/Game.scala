sealed trait Game {
  def field: IndexedSeq[IndexedSeq[Option[Player]]]
}
object Game {
  class On(val thatMove: Player, val field: IndexedSeq[IndexedSeq[Option[Player]]]) extends Game {
    def move(x: Int, y: Int): Game =
      if(fieldOut(x) || fieldOut(y)) this
      else if(field(x)(y).isDefined) this
      else field.updated(x, field(x).updated(y, Some(thatMove))) match {
        case nextField @ Win(player) => new Over(Some(player), nextField)
        case nextField @ Full() => new Over(None, nextField)
        case nextField => new On(thatMove.reverse, nextField)
      }

    private def fieldOut(a: Int) = a < 0 || a > field.length

    object Win {
      def unapply(field: IndexedSeq[IndexedSeq[Option[Player]]]): Option[Player] = None
    }

    object Full {
      def unapply[T](seq: Seq[Seq[Option[T]]]): Boolean = seq.forall( _.forall( _.isDefined ) )
    }
  }

  class Over(val winner: Option[Player], val field: IndexedSeq[IndexedSeq[Option[Player]]]) extends Game {
    val loser = winner.map( _.reverse )
  }

  def apply(n: Int): Game = new On(Crosses, IndexedSeq.fill(n, n)(None))

}

sealed trait Player {
  def reverse: Player = this match {
    case Crosses => Noughts
    case Noughts => Crosses
  }
}
case object Crosses extends Player// Крестики
case object Noughts extends Player// Нолики


