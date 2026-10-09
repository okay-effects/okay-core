package okay.std

import okay.core.*

/** CHOICE, multi-shot: `among(as)` is answered once per element, the continuation resumed for each, and the
 * delimiter answers every value the body came to, in order */
enum Choose[+A]:
  case Among[A](as: Seq[A]) extends Choose[A]

def among[A](as: Seq[A])(using c: Effects, has: Choose :? c.Effs): c.Prog[A] = perform[Choose, A](Choose.Among(as))
/** `choose[A](body)`: every value of the body, one per path through its choices */
def choose[A](using c: Effects)(body: c.Body[Choose, Seq[A], A]): c.Prog[Seq[A]] =
  Effects.handle(new Handler[c.Prog, Choose, A, Seq[A]]:
    // appended to the empty one: `Vector(a)` makes an array of `A` first, its class tag looked up each time
    def ret(a: A): Seq[A] = Vector.empty[A] :+ a
    // the paths' values joined in a Vector: appending a path's to the ones before copies nothing of them — a
    // List or a Seq's `++` copied all the values so far for each path, quadratic in their number
    def apply[X](op: Choose[X], k: X => c.Prog[Seq[A]]): c.Prog[Seq[A]] = op match
      // element by element, each path's values appended as it ends: one closure an element, no program of all the
      // elements built first — a fold bound them all before the first ran, two closures and a frame each
      case Choose.Among(as) =>
        val xs = as match
          case is: IndexedSeq[X @unchecked] => is
          case _ => as.toVector
        def from(i: Int, acc: Vector[A]): c.Prog[Seq[A]] =
          if i == xs.length then c.pure[c.S, Seq[A]](acc) else k(xs(i)).flatMap(r => from(i + 1, acc ++ r))
        from(0, Vector.empty))(body)
