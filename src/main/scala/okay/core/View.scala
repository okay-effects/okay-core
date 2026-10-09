package okay.core

/** A CONTEXT SEEN OVER ANOTHER ROW: `c`, whose row is `c.Effs`, seen as one over `RV` — each operation's path
 * translated by `t`, everything else `c`'s own, its delimiters' contexts seen so too. What a program built for one
 * row runs in when a handler has made the row another: `Program.handle`, a handler of an effect anywhere in a row */
class View[RV <: Row, C <: Effects](val c: C, val t: Tr[RV, c.Effs]) extends Effects:
  type M[I, O, A] = c.M[I, O, A]
  type Effs = RV
  type S = c.S
  type Within[I, O, A] = c.Within[I, O, A]
  type Handling[E[_], I, O, A] = c.Handling[E, I, O, A]
  type Handled[E[_], S2] = View[E +: RV, c.Handled[E, S2]]
  type Reset[S2] = View[RV, c.Reset[S2]]
  type Answer[S2] = View[RV, c.Answer[S2]]

  def pure[S2, A](a: A): M[S2, S2, A] = c.pure(a)
  def delay[I, O, A](m: => M[I, O, A]): M[I, O, A] = c.delay(m)
  extension [I, O, A](m: M[I, O, A])
    def flatMap[I2, B](f: A => M[I2, I, B]): M[I2, O, B] = c.flatMap(m)(f)
    def map[B](f: A => B): M[I, O, B] = c.map(m)(f)

  /** the paths translated, by their depth in the row: a path is made anew where it is found — the compiler's given,
   * each call — but the same path is the same translation, so made once a view, not an object each operation. A
   * path's depth is at most the row's length; past `View.Memos` deep, translated each time */
  private val memos = new Array[View.Memo[RV, c.Effs, ?] | Null](View.Memos)
  private def translate[E[_]](in: In[E, RV]): In[E, c.Effs] =
    val d = View.depth(in)
    if d >= View.Memos then t(in)
    else memos(d) match
      case m: View.Memo[RV, c.Effs, e] => In.same(m.in, in) match
        case null => t(in)
        case same => same.in(m.out)
      case _ =>
        val out = t(in)
        memos(d) = View.Memo(in, out)
        out
  def perform[E[_], S2, X](op: E[X], in: In[E, RV]): M[S2, S2, X] = c.perform(op, translate(in))
  def handle[E[_], A, Ans](h: Handler[Prog, E, A, Ans])(body: Handled[E, Ans] ?=> Handling[E, Ans, Ans, A]): M[S, S, Ans] =
    c.handle(h)(hc ?=> body(using View(hc, Tr.under[E, RV, c.Effs](t))))
  def intercept[E[_], A, Ans](in: In[E, RV], h: Handler[Prog, E, A, Ans])(body: Reset[Ans] ?=> Within[Ans, Ans, A]): M[S, S, Ans] =
    c.intercept(translate(in), h)(r ?=> body(using View(r, t)))
  def answer[S2]: Answer[S2] = View(c.answer[S2], t)
  def shift[I, O, A](f: (A => M[O, O, I]) => M[O, O, O], same: (I =:= O) | Null): M[I, O, A] = c.shift(f, same)
  def reset[S2, O](body: Reset[S2] ?=> Within[S2, O, S2]): M[S, S, O] = c.reset[S2, O](r ?=> body(using View(r, t)))
  // a view of `c` itself: its monad `c`'s, which a view of any `C` would not say
  type Needing[Es <: AnyKind] = View[RV, c.type] & Needs[Es]
  def needing[Es <: AnyKind](p: Paths[Es, RV]): View[RV, c.type] & Needs[Es] = View.Needy[RV, c.type, Es](c, t, p)
  def top: Top = c.top

object View:
  /** a path and its translation, of one effect */
  final class Memo[RV <: Row, RC <: Row, E[_]](val in: In[E, RV], val out: In[E, RC])
  /** how deep a path a view keeps the translation of */
  final val Memos = 8
  /** a path's depth: how many effects before its own */
  def depth[E[_], R <: Row](in: In[E, R]): Int =
    @scala.annotation.tailrec def go[E2[_], R2 <: Row](p: In[E2, R2], n: Int): Int = p match
      case _: In.Here[e, t] => n
      case th: In.There[e, x, t] => go(th.in, n + 1)
    go(in, 0)
  /** a view with the paths of the effects a program needs */
  final class Needy[RV <: Row, C <: Effects, Es <: AnyKind](c0: C, t0: Tr[RV, c0.Effs], p: Paths[Es, RV]) extends View[RV, C](c0, t0) with Needs[Es]:
    def find[E[_]](r: Req[E, Es]): In[E, RV] = p.find(r)
