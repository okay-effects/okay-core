package okay

/** THE USER'S OKAY: `import okay.*`. A program's type is `A ! E1 + E2 + …` — `!`, `+` and `%` all it needs — its
 * operations and handlers the standard effects', `perform` one's own, `run` it. Everything else — rows, paths,
 * places, the kernel — is `okay.core`'s and `okay.one`'s own business */

/** `A ! E1 + E2`: a program of value `A`, in any place that has the effects it names, in any order */
infix type ![+A, F[_]] = okay.core.![A, F]
/** effects joined: `Reader % Int + Async` */
infix type +[F[_], G[_]] = okay.core.+[F, G]
/** an effect of two parameters, its first given: `Reader % Int` */
infix type %[F[_, _], S] = okay.core.%[F, S]

export okay.std.{Reader, ask, reader, local, State, get, put, modify, state, cell, Writer, tell, writer,
  Throws, raise, throws, handleWith, Choose, among, choose, first, Emit, yield_, collect, generate, PState, pstate,
  Source, Stream, Generator, Step, stream}
// for one's own effects and handlers, and a program with its row in its type
/** `E :? R`, the effect `E` in the row `R`; `Has[E]` as a bound */
infix type :?[E[_], R <: okay.core.Row] = okay.core.:?[E, R]
type Has[E[_]] = okay.core.Has[E]
type Row = okay.core.Row
infix type +:[E[_], T <: okay.core.Row] = okay.core.+:[E, T]
type Pure = okay.core.Pure
type Eff[R <: okay.core.Row, +A] = okay.core.Eff[R, A]
type Shifts[S, T <: okay.core.Row] = okay.core.Shifts[S, T]
/** `Atm[A, X, Y, Es]`: a program needing effects under a `$`, its answer moved; `Atm.shift0`, `Atm.dollar` */
type Atm[+A, X[_ <: okay.core.Row], Y[_ <: okay.core.Row], Es[_]] = okay.core.Atm[A, X, Y, Es]
val Atm: okay.core.Atm.type = okay.core.Atm
/** a `$`'s answers: `Hole[X, A]`, a hole of `X` in a program of `A` over the row outside; `Fixed[A]`, unchanged */
type Hole[X, A] = okay.core.Hole[X, A]
type Fixed[A] = okay.core.Fixed[A]
type UnderAt[E[_], T <: okay.core.Row, I, O, A] = okay.core.UnderAt[E, T, I, O, A]
export okay.core.{perform, pure, unit, defer, handle, handleIn, handleAt, translate, intercept, run, Handler, Answering,
  Value, Under, Place, Needs, Delimited, Path, RowEffects, Op}
export okay.core.Control.{reset, shift, shift0, lower}

/** `effect(op)`: an operation of one's own effect, a program — okay's word for `perform` */
def effect[E[_], X](op: E[X]): okay.core.Op[E, X] = okay.core.perform(op)
extension [E[_], X](op: E[X])
  /** `op.perform`: the same, written after the operation */
  @scala.annotation.targetName("performOp")
  def perform: okay.core.Op[E, X] = okay.core.perform(op)
