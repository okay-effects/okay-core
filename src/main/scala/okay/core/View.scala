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

  def perform[E[_], S2, X](op: E[X], in: In[E, RV]): M[S2, S2, X] = c.perform(op, t(in))
  def handle[E[_], A, Ans](h: Handler[Prog, E, A, Ans])(body: Handled[E, Ans] ?=> Handling[E, Ans, Ans, A]): M[S, S, Ans] =
    c.handle(h)(hc ?=> body(using View(hc, Tr.under[E, RV, c.Effs](t))))
  def intercept[E[_], A, Ans](in: In[E, RV], h: Handler[Prog, E, A, Ans])(body: Reset[Ans] ?=> Within[Ans, Ans, A]): M[S, S, Ans] =
    c.intercept(t(in), h)(r ?=> body(using View(r, t)))
  def answer[S2]: Answer[S2] = View(c.answer[S2], t)
  def shift[I, O, A](f: (A => M[O, O, I]) => M[O, O, O], same: (I =:= O) | Null): M[I, O, A] = c.shift(f, same)
  def reset[S2, O](body: Reset[S2] ?=> Within[S2, O, S2]): M[S, S, O] = c.reset[S2, O](r ?=> body(using View(r, t)))
  // a view of `c` itself: its monad `c`'s, which a view of any `C` would not say
  type Needing[Es <: AnyKind] = View[RV, c.type] & Needs[Es]
  def needing[Es <: AnyKind](p: Paths[Es, RV]): View[RV, c.type] & Needs[Es] = View.Needy[RV, c.type, Es](c, t, p)
  def top: Top = c.top

object View:
  /** a view with the paths of the effects a program needs */
  final class Needy[RV <: Row, C <: Effects, Es <: AnyKind](c0: C, t0: Tr[RV, c0.Effs], p: Paths[Es, RV]) extends View[RV, C](c0, t0) with Needs[Es]:
    def find[E[_]](r: Req[E, Es]): In[E, RV] = p.find(r)
