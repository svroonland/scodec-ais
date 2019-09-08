package nl.vroste.ais

import scodec.Attempt

private[ais] object AttemptExtensions {
  implicit class AttemptSequence(val s: Attempt.type) extends AnyVal {
    def sequence[T, U >: T](attempts: Seq[Attempt[T]]): Attempt[Seq[U]] =
      attempts.foldLeft(Attempt.successful(Seq.empty[U])) {
        case (acc, curr) =>
          acc.flatMap(a => curr.map(a :+ _))
      }

    def traverse[T, U](values: Seq[T])(f: T => Attempt[U]): Attempt[Seq[U]] =
      sequence(values.map(f))
  }
}
