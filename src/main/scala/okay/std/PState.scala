package okay.std

import okay.core.*

/** PSTATE: a state whose TYPE may change, carried by the ANSWER TYPE — Danvy–Filinski's state passing, the type
 * moved by Asai–Kameyama's answer-type modification. Not an effect of the row: no operation, no handler; `get`
 * and `put` are captures up to the run's delimiter, whose answer is the rest of the run given the state, `St`.
 * `put` of another type moves the answer from `St[S2]` to `St[S]`, and the binds compose the moves — a `get` of
 * the wrong type, a `put` out of order, does not compile. Written against `Control`, any monad with it */
/** the answer of a stateful program over the monad `M`: given the state `S`, the rest of the run, of value `V`, at
 * the answer `Q` outside */
type St[M[_, _, _], Q, S, V] = S => M[Q, Q, V]

/** the operations of one run, over the control `c`, in the run's reset `r`: the run's value `V` */
final class PState[C <: Control & Singleton, V](val c: C)(val r: Control { type M[I, O, A] = c.Within[I, O, A]; type Effs = c.Effs }):
  type At[S] = St[c.M, c.S, S, V]
  /** the state, its type `S` unchanged: `shift(k => s => k(s)(s))` */
  def get[S]: c.Within[At[S], At[S], S] =
    r.shift[At[S], At[S], S](k => r.pure((s: S) => c.reset[At[S], At[S]](k(s)).flatMap(f => f(s))), <:<.refl)
  /** a state of another type, `S2`, for the rest; the state before was `S`: `shift(k => _ => k(())(s2))` */
  def put[S]: PutFrom[S] = PutFrom[S]()
  final class PutFrom[S]:
    def apply[S2](s2: S2): c.Within[At[S2], At[S], Unit] =
      // the rest's value, the run from `S2` on, to `S`'s answer — so it is delimited as the body it is
      r.shift[At[S2], At[S], Unit](k => r.pure((s: S) => c.reset[At[S], At[S]](k(()).map(f => ((_: S) => f(s2)): At[S])).flatMap(g => g(s))), null)

/** `pstate(s0)(st => body)`: the body run from the state `s0`, of type `S0`, to a state of type `S1`, in the run's
 * reset; the last state and the value */
def pstate[S0](s0: S0)[S1, W](using c: Control)(body: c.Reset[St[c.M, c.S, S1, (S1, W)]] ?=> PState[c.type, (S1, W)] => c.Within[St[c.M, c.S, S1, (S1, W)], St[c.M, c.S, S0, (S1, W)], W])
    : c.Prog[(S1, W)] =
  c.reset[St[c.M, c.S, S1, (S1, W)], St[c.M, c.S, S0, (S1, W)]](r ?=>
    body(PState(c)(r)).map(w => (s1: S1) => c.pure[c.S, (S1, W)]((s1, w)))).flatMap(f => f(s0))
