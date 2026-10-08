package okay.std

import okay.core.*

/** PSTATE: a state whose TYPE may change, carried by the ANSWER TYPE — Danvy–Filinski's state passing, the type
 * moved by Asai–Kameyama's answer-type modification. Not an effect of the row: no operation, no handler; `get`
 * and `put` are captures up to the run's delimiter, whose answer is the rest of the run given the state, `St`.
 * `put` of another type moves the answer from `St[S2]` to `St[S]`, and the binds compose the moves — a `get` of
 * the wrong type, a `put` out of order, does not compile. Written against `Control`, any monad with it */
/** the answer of a stateful program over the monad `M`: given the state `S`, the rest of the run, of value `V`, at
 * the answer `Q` outside */
type St[M[_ <: Row, _, _, _], R <: Row, Q, S, V] = S => M[R, Q, Q, V]

/** the operations of one run, over the control `c`: the row `R`, the answer `Q` outside it, the run's value `V` */
final class PState[C <: Control & Singleton, R <: Row, Q, V](val c: C):
  type At[S] = St[c.M, R, Q, S, V]
  private given C = c
  /** the state, its type `S` unchanged: `shift(k => s => k(s)(s))` */
  def get[S]: c.M[R, At[S], At[S], S] =
    c.shift(k => c.pure((s: S) => c.reset[R, Q, At[S], At[S], At[S]](k(s), identity).flatMap(f => f(s))))
  /** a state of another type, `S2`, for the rest; the state before was `S`: `shift(k => _ => k(())(s2))` */
  def put[S]: PutFrom[S] = PutFrom[S]()
  final class PutFrom[S]:
    def apply[S2](s2: S2): c.M[R, At[S2], At[S], Unit] =
      // the rest's value, the run from `S2` on, to `S`'s answer — so it is delimited as the body it is
      c.shift(k => c.pure((s: S) => c.reset[R, Q, At[S], At[S], At[S]](k(()).map(f => ((_: S) => f(s2)): At[S]), identity).flatMap(g => g(s))))

/** `pstate(s0)(st => body)`: the body run from the state `s0`, of type `S0`, to a state of type `S1`; the last
 * state and the value */
def pstate[S0](s0: S0)[S1, W](using c: Control)(body: PState[c.type, c.R, c.S, (S1, W)] => c.M[c.R, St[c.M, c.R, c.S, S1, (S1, W)], St[c.M, c.R, c.S, S0, (S1, W)], W])
    : c.M[c.R, c.S, c.S, (S1, W)] =
  c.reset(body(PState(c)), (w: W) => (s1: S1) => c.pure[c.R, c.S, (S1, W)]((s1, w))).flatMap(f => f(s0))
