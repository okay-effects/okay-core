package okay.std

import okay.core.*

/** STATE: `get` and `put` answered in place from a cell, one per RUN of `state(s0)(body)` — the cell is made when
 * the program is stepped, not when it is built, so a program run twice starts from `s0` twice. A resumption
 * shares the cell: a body resumed twice sees one state, the second resumption the first's last */
enum State[S, +A]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]

def get[S](using c: Effects, has: State % S :? c.R): c.M[c.R, c.S, c.S, S] = perform[State % S, S](State.Get[S]())
def put[S](s: S)(using c: Effects, has: State % S :? c.R): c.M[c.R, c.S, c.S, Unit] = perform[State % S, Unit](State.Put(s))
/** the state changed by `f`; the new state */
def modify[S](f: S => S)(using c: Effects, has: State % S :? c.R): c.M[c.R, c.S, c.S, S] =
  get[S].flatMap(s => { val n = f(s); put[S](n).map(_ => n) })
/** `state[A](s0)(body)`: the last state and the value */
def state[A](using c: Effects)[S](s0: S)(body: c.Handled[State % S, (S, A)] ?=> c.Inner[State % S +: c.R, (S, A), (S, A), A]): c.M[c.R, c.S, c.S, (S, A)] =
  c.delay:
    Effects.handle(new Answering[c.M, State % S, c.R, c.S, A, (S, A)]:
      private var s: S = s0
      def ret(a: A): (S, A) = (s, a)
      def value[X](op: State[S, X]): X = op match
        case State.Get() => s
        case State.Put(n) => s = n)(body)
