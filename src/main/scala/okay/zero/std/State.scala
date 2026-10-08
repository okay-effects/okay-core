package okay.zero.std

import okay.zero.*

/** STATE: `get` and `put` answered in place from a cell, one per RUN of `state(s0)(body)` — made when the program
 * is stepped, not when it is built, so a program run twice starts from `s0` twice. A resumption shares the cell */
enum State[S, +A]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]

def get[S](using c: Ctx, has: State % S :? c.R): c.P[S] = perform[State % S, S](State.Get[S]())
def put[S](s: S)(using c: Ctx, has: State % S :? c.R): c.P[Unit] = perform[State % S, Unit](State.Put(s))
/** the state changed by `f`; the new state */
def modify[S](f: S => S)(using c: Ctx, has: State % S :? c.R): c.P[S] =
  get[S].flatMap(s => { val n = f(s); put[S](n).map(_ => n) })
/** `state[A](s0)(body)`: the last state and the value */
def state[A](using c: Ctx)[S](s0: S)(body: c.Body[State % S, (S, A), A]): c.P[(S, A)] =
  delay:
    handle(new Answering[c.P, State % S, A, (S, A)]:
      private var s: S = s0
      def ret(a: A): (S, A) = (s, a)
      def value[X](op: State[S, X]): X = op match
        case State.Get() => s
        case State.Put(n) => s = n)(body)
