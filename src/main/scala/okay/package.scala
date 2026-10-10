package okay

/** THE USER'S OKAY: `import okay.*`. A program's type is `A ! E1 + E2 + …` — `!`, `+` and `%` all it needs — its
 * operations and handlers the standard effects', `perform` one's own, `run` it on `Cont0` (`runOn(top)` on another
 * monad). Everything else — contexts, rows, paths, the monads — is `okay.core`'s and the monads' own business */

/** `A ! E1 + E2`: a program of value `A`, in any context that has the effects it names, in any order */
infix type ![A, F[_]] = okay.core.![A, F]
/** effects joined: `Reader % Int + Async` */
infix type +[F[_], G[_]] = okay.core.+[F, G]
/** an effect of two parameters, its first given: `Reader % Int` */
infix type %[F[_, +_], S] = okay.core.%[F, S]

export okay.std.{Reader, ask, reader, local, State, get, put, modify, state, Writer, tell, writer,
  Throws, raise, throws, handleWith, Choose, among, choose, Emit, yield_, collect, generate,
  Source, Stream, Generator, Step, stream, PState, pstate, Programs}
// for one's own effects and handlers, and a program with its row in its type
/** `E :? R`, the effect `E` in the row `R`: what a context-written program asks for */
infix type :?[E[_], R <: okay.core.Row] = okay.core.:?[E, R]
type Row = okay.core.Row
infix type +:[E[_], T <: okay.core.Row] = okay.core.+:[E, T]
type Pure = okay.core.Pure
export okay.core.{perform, pure, Effects, Handler, Answering, Program, Top}
export okay.core.Control.{reset, shift}

/** `effect(op)`: an operation of one's own effect, a program — okay's word for `perform` */
def effect[E[_], X](op: E[X]): X ! E = okay.core.perform(op)
extension [E[_], X](op: E[X])
  /** `op.perform`: the same, written after the operation */
  def perform: X ! E = okay.core.perform(op)

/** `run(p)`: a program with nothing left to handle, run on `Cont0` — its value */
def run[A](p: (c: okay.core.Effects { type Effs = okay.core.Pure; type S = A }) ?=> c.Prog[A]): A =
  runOn(okay.zero.top)(p)
/** `runOn(top)(p)`: the same on the monad of `top` — `okay.zero.top`, `okay.cont.top` */
def runOn[A](top: okay.core.Top)(p: (c: okay.core.Effects { type Effs = okay.core.Pure; type S = A }) ?=> c.Prog[A]): A =
  top.run(p(using top.at[A]))
