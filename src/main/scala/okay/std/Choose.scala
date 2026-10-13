package okay.std

import okay.core.*
import okay.one.handler.Deep

/** CHOICE, multi-shot: `among(as)` is answered once per element, the rest resumed for each — `k` data, resumed as
 * many times as there are elements */
export okay.one.std.Choose

def among[A](as: Seq[A]): Op[Choose, A] = perform(Choose.Among(as))
/** no value: this path dropped */
def fail[A]: Op[Choose, A] = among(Nil)
/** `choose(body)`: every value of the body, one per path through its choices, in order */
def choose[A, T <: Row](body: Under[Choose, T, A]): Eff[T, Seq[A]] =
  handleIn(new Deep[Choose, T, A, Seq[A]]:
    // appended to the empty one: `Vector(a)` makes an array of `A` first, its class tag looked up each time
    def ret(a: A): Eff[T, Seq[A]] = pure(Vector.empty[A] :+ a)
    // the paths' values joined in a Vector: appending a path's to the ones before copies nothing of them; element by
    // element, each path's values appended as it ends — no program of all the elements built first
    def apply[X](op: Choose[X], k: X => Eff[T, Seq[A]]): Eff[T, Seq[A]] = op match
      case Choose.Among(as) =>
        val xs = as match
          case is: IndexedSeq[X @unchecked] => is
          case _ => as.toVector
        def from(i: Int, acc: Vector[A]): Eff[T, Seq[A]] =
          if i == xs.length then pure(acc) else k(xs(i)).flatMap(r => from(i + 1, acc ++ r))
        from(0, Vector.empty))(body)
/** `first(body)`: the first value the body comes to, its choices tried in order, the rest not tried */
def first[A, T <: Row](body: Under[Choose, T, A]): Eff[T, Option[A]] =
  handleIn(new Deep[Choose, T, A, Option[A]]:
    def ret(a: A): Eff[T, Option[A]] = pure(Some(a))
    def apply[X](op: Choose[X], k: X => Eff[T, Option[A]]): Eff[T, Option[A]] = op match
      case Choose.Among(as) =>
        def from(rest: Seq[X]): Eff[T, Option[A]] =
          if rest.isEmpty then pure(None) else k(rest.head).flatMap(r => if r.isDefined then pure(r) else from(rest.tail))
        from(as))(body)
