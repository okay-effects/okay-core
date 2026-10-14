package okay.core.std

import okay.core.*
import okay.one.handler.Answering as OneAnswering

/** STATE: `get` and `put` answered in place. Two handlers: `state`, the kernel's — a register of the machine, a `k`
 * taken past it with the state as it was then, each path of a choice its own; `cell`, one cell a RUN — made when the
 * program is stepped, not when it is built — shared by every resumption: the second sees the first's last */
export okay.one.std.State

def get[S]: Op[State % S, S] = perform(State.Get[S]())
def put[S](s: S): Op[State % S, Unit] = perform(State.Put(s))
/** the state changed by `f`; the new state — a program needing the state, as any */
def modify[S](f: S => S): S ! State % S = get[S].flatMap(s => { val n = f(s); put[S](n).map(_ => n) })
/** `state(s0)(body)`: the last state and the value — each path its own */
def state[S, A, T <: Row](s0: S)(body: Under[State % S, T, A]): Eff[T, (S, A)] = handleIn(State[S, T, A](s0))(body)
/** `cell(s0)(body)`: the same from a cell, one a run, every resumption's */
def cell[S, A, T <: Row](s0: S)(body: Under[State % S, T, A]): Eff[T, (S, A)] = defer:
  handleIn(new OneAnswering[State % S, T, A, (S, A)]:
    private var s: S = s0
    def ret(a: A): Eff[T, (S, A)] = pure((s, a))
    def value[X](op: State[S, X]): X = op match
      case State.Get() => s
      case State.Put(n) => s = n)(body)
