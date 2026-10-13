package okay.std

import okay.core.*
import okay.one.Cont0

/** PSTATE: a state whose TYPE may change, carried by the ANSWER TYPE — the kernel's, in control (`okay.one.std.PState`):
 * `get` and `put` shift0s up to the run's `$`, a `put` of another type moving the answer; a `get` of the wrong type
 * does not compile. Here as a program of effects: the run lowered, its operations of the row answered around it */
export okay.one.std.PState

/** `pstate(s0)(st => body)`: the body run from the state `s0`, of type `S0`, to one of type `S1`; the last state and
 * the value */
def pstate[R <: Row, S0, S1, W](s0: S0)(body: (st: PState[R, (S1, W), (S1, W)]) =>
    Cont0[okay.one.Control[R], st.P[S1], st.P[S0], W]): Eff[R, (S1, W)] =
  Control.lower(PState.pstate[R, (S1, W), S0, S1, W](s0)(body))
