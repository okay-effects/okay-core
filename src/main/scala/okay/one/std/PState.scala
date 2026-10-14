package okay.one.std

import okay.one.*
import okay.one.Cont0.*
import okay.one.row.*

/** PSTATE: a state whose TYPE may change, carried by the ANSWER TYPE — Danvy–Filinski's state passing, the type moved
 * by Asai–Kameyama's answer-type modification, under a `$`. Not an effect of the row: `get` and `put` are shift0s up
 * to the run's `$`, whose answer is the rest of the run given the state, `At`. A `put` of another type
 * moves the answer from `At[S2]` to `At[S]`: a `get` of the wrong type, a `put` out of order, does not compile */
final class PState[R <: Row, Q, F]:
  /** the rest of the run given the state `S`: its program, of the run's value `F`, at the answer `Q` outside */
  type At[S] = S => Cont0[R, Q, Q, F]
  /** a program of the answer `At[S]` */
  type P[S] = Cont0[R, Q, Q, At[S]]

  /** the state, its type `S` unchanged: `shift0(k => s => k(s)(s))` */
  def get[S]: Cont0[Delim[R], P[S], P[S], S] =
    Shift0[Delim[R], R, Q, Q, At[S], P[S], S](Nearest.Here(), k => Return((s: S) => Bind(k(s), (f: At[S]) => f(s))))

  /** a state of another type, `S2`, for the rest; the state before, `S`: `shift0(k => _ => k(())(s2))` */
  def put[S]: PutFrom[S] = PutFrom[S]()
  final class PutFrom[S]:
    def apply[S2](s2: S2): Cont0[Delim[R], P[S2], P[S], Unit] =
      Shift0[Delim[R], R, Q, Q, At[S2], P[S], Unit](Nearest.Here(), k => Return((_: S) => Bind(k(()), (f: At[S2]) => f(s2))))

object PState:
  /** `pstate(s0)(st => body)`: the body run from the state `s0`, of type `S0`, to one of type `S1`, under its `$` over
   * the row `R`: the last state and the value, a program over `R` at any answer `Q` */
  def pstate[R <: Row, Q, S0, S1, W](s0: S0)(body: (st: PState[R, Q, (S1, W)]) =>
      Cont0[Delim[R], st.P[S1], st.P[S0], W]): Cont0[R, Q, Q, (S1, W)] =
    val st = PState[R, Q, (S1, W)]()
    Bind(Dollar[R, st.P[S1], Q, Q, st.At[S0], W](body(st),
      (w: W) => Return[R, Q, st.At[S1]]((s1: S1) => Return[R, Q, (S1, W)]((s1, w)))),
      (f: st.At[S0]) => f(s0))
