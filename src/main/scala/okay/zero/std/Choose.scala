package okay.zero.std

import okay.zero.*

/** CHOICE, multi-shot: `among(as)` answered once per element, the rest resumed for each; the handler answers every
 * value the body came to, in order */
enum Choose[+A]:
  case Among[A](as: Seq[A]) extends Choose[A]

def among[A](as: Seq[A])(using c: Ctx, has: Choose :? c.R): c.P[A] = perform[Choose, A](Choose.Among(as))
/** `choose[A](body)`: every value of the body, one per path through its choices */
def choose[A](using c: Ctx)(body: c.Body[Choose, Seq[A], A]): c.P[Seq[A]] =
  handle(new Handler[c.P, Choose, A, Seq[A]]:
    def ret(a: A): Seq[A] = Vector(a)
    // the paths' values joined in a Vector: appending a path's copies nothing of the ones before
    def apply[X](op: Choose[X], k: X => c.P[Seq[A]]): c.P[Seq[A]] = op match
      case Choose.Among(as) => as.foldLeft(pure[c.R, c.J, c.T, Seq[A]](Vector.empty))((acc, x) => acc.flatMap(v => k(x).map(v ++ _))))(body)
