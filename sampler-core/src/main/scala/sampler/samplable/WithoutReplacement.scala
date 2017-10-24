package sampler.samplable

import cats.data.State
import sampler.maths.Random

case class Draw[R, T](remainder: R, drawnCounts: Map[T, Int])

/** Trait for sampling without replacement
  *
  * Wraps some kind of collection [R] of items of type [T], and allows them to be sampled
  * without replacement.
  *
  */
trait WithoutReplacement[R, T] {
  type Counts = Map[T, Int]

  val items: R

  def numRemaining: Int

  def empty: State[R, Counts]

  /** Draw without replacement
    *
    * @return A [[sampler.samplable.Draw[R,T]] containing the draw counts and remainder left
    */
  def draw(n: Int = 1)(implicit r: Random): Draw[R, T] = {
    val state = drawState(n)
    val (remainder, drawn) = state.run(items)
    Draw(remainder, drawn)
  }

  /*
   * State which removes a single item
   */
  protected def removeOne(soFar: Counts)(implicit r: Random): State[R, Counts]

  /*
   * State which removes n items
   */
  protected def drawState(n: Int = 1)(implicit r: Random): State[R, Counts] = for (
    selection <- {
      assert(numRemaining >= n, "Not enough items in the collection to sample the desired quantity")
      (1 to n).foldLeft(empty)((accState, _) => accState.flatMap(removeOne))
    }
  ) yield selection
}

trait WithoutReplacementImplicits {

  implicit class WithoutReplacemenMap[T](val items: Map[T, Int]) extends WithoutReplacement[Map[T, Int], T] {
    def empty = State.pure(Map[T, Int]())

    def numRemaining = items.values.sum

    def removeOne(soFar: Counts)(implicit r: Random): State[Counts, Counts] = for (
      item <- State[Counts, T] { s =>
        val (items, counts) = s.toIndexedSeq.unzip
        val countIndex = r.nextInt(s.values.sum)
        val selectedIndex = counts
          .view
          .scanLeft(0)(_ + _)
          .drop(1)
          .indexWhere(_ > countIndex)
        val selected: T = items(selectedIndex)
        (s.updated(selected, s(selected) - 1), selected)
      }
    ) yield soFar.updated(item, soFar.getOrElse(item, 0) + 1)
  }

  implicit class WithoutReplacemenSeq[T](val items: IndexedSeq[T]) extends WithoutReplacement[IndexedSeq[T], T] {
    type Remain = IndexedSeq[T]

    def empty = State.pure(Map[T, Int]())

    def numRemaining = items.size

    def removeOne(soFar: Counts)(implicit r: Random): State[Remain, Counts] =
      for (
        remaining <- State.get[Remain];
        index = r.nextInt(remaining.size);
        item <- state(remaining(index));
        _ <- put(remaining.patch(index, Nil, 1))
      ) yield soFar.updated(item, soFar.getOrElse(item, 0) + 1)
  }

}