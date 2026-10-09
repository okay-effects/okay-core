package okay.std

import okay.core.*

/** CHOICE, multi-shot: `among(as)` is answered once per element, the continuation resumed for each, and the
 * delimiter answers every value the body came to, in order */
enum Choose[+A]:
  case Among[A](as: Seq[A]) extends Choose[A]

def among[A](as: Seq[A])(using c: Effects, has: Choose :? c.R): c.P[A] = perform[Choose, A](Choose.Among(as))
/** `choose[A](body)`: every value of the body, one per path through its choices */
def choose[A](using c: Effects)(body: c.Body[Choose, Seq[A], A]): c.P[Seq[A]] =
  Effects.handle(new Handler[c.M, Choose, c.R, c.S, A, Seq[A]]:
    def ret(a: A): Seq[A] = Vector(a)
    // the paths' values joined in a Vector: appending a path's to the ones before copies nothing of them — a
    // List or a Seq's `++` copied all the values so far for each path, quadratic in their number
    def apply[X](op: Choose[X], k: X => c.P[Seq[A]]): c.P[Seq[A]] = op match
      case Choose.Among(as) =>
        as.foldLeft(c.pure[c.R, c.S, Seq[A]](Vector.empty))((acc, x) => acc.flatMap(v => k(x).map(v ++ _))))(body)
