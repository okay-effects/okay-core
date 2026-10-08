package okay.zero.std

import okay.zero.*

/** PSTATE: a state whose TYPE may change, carried by the ANSWER TYPE — Danvy–Filinski's state passing, the type
 * moved by Asai–Kameyama's answer-type modification. Not an effect of the row: `get` and `put` are shifts up to
 * the run's reset, whose answer is the rest of the run given the state. `put` of another type moves the answer
 * from `At[S2]` to `At[S]`, the binds compose the moves — a `get` of the wrong type, a `put` out of order, does
 * not compile */
final class PState[C <: Ctx & Singleton, V](val c: C):
  /** the answer inside the run: given the state `S`, the rest of the run outside, of value `V` */
  type At[S] = S => c.P[V]
  /** the state, its type `S` unchanged: `shift(k => s => k(s)(s))` */
  def get[S]: Eff[c.R, c.Inner, At[S], At[S], S] =
    control[c.R, c.Inner, S, At[S], At[S]](k => pure((s: S) => reset[At[S]](using c)[At[S]](k(s)).flatMap(f => f(s))), <:<.refl)
  /** a state of another type, `S2`, for the rest; the state before was `S`: `shift(k => _ => k(())(s2))` */
  def put[S]: PutFrom[S] = PutFrom[S]()
  final class PutFrom[S]:
    def apply[S2](s2: S2): Eff[c.R, c.Inner, At[S2], At[S], Unit] =
      control[c.R, c.Inner, Unit, At[S2], At[S]](k =>
        pure((s: S) => reset[At[S]](using c)[At[S]](k(()).map(f => ((_: S) => f(s2)): At[S])).flatMap(g => g(s))), null)

/** `pstate(s0)(st => body)`: the body run from the state `s0`, of type `S0`, to a state of type `S1`; the last state
 * and the value. The body in the run's context, given it: an operation there at the answer the run has moved to,
 * `performAt[st.At[S]](op)` */
def pstate[S0](s0: S0)[S1, W](using c: Ctx)
    (body: c.Reset[S1 => c.P[(S1, W)]] ?=> PState[c.type, (S1, W)] => Eff[c.R, c.Inner, S1 => c.P[(S1, W)], S0 => c.P[(S1, W)], W])
    : c.P[(S1, W)] =
  reset[S1 => c.P[(S1, W)]](using c)[S0 => c.P[(S1, W)]](body(PState(c)).map(w => (s1: S1) => pure((s1, w)))).flatMap(f => f(s0))
