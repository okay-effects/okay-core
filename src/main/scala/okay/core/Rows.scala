package okay.core

import okay.one.row.{+:, In, Pure, Row}

/** THE KERNEL'S ROWS, as they are: `Pure`, `E +: T`; an effect's place `In[E, R]`; `Removed[E, R, T]` — aliases,
 * not exports: scalac 2.13's TASTy reader cannot read an export of a sealed trait (`addChild inapplicable`) */
type Row = okay.one.row.Row
type Pure = okay.one.row.Pure
infix type +:[E[_], T <: Row] = okay.one.row.+:[E, T]
type In[E[_], R <: Row] = okay.one.row.In[E, R]
val In: okay.one.row.In.type = okay.one.row.In
type Removed[E[_], R <: Row, T <: Row] = okay.one.row.Removed[E, R, T]
val Removed: okay.one.row.Removed.type = okay.one.row.Removed
/** the kernel's program of effects over the row `R` — `okay.one`'s `A ! R`, named the other way round here, the row
 * first, as a monad is: `Eff[R, A]`. `A ! Es` (below) is the facade's: the effects named, the row the handlers' */
type Eff[R <: Row, +A] = okay.one.Cont0[R, Nothing, Nothing, A]   // `okay.one`'s `A ! R`, written out: Scala 2 sees no top-level alias

/** THE KERNEL'S WORDS FOR SCALA 2, which cannot read an inline definition (the kernel's `perform`, its `In.here`):
 * `perform` by a path given, and the paths as implicits — `import okay.core.Ops._` */
object Ops:
  def perform[E[_], R <: Row, X](op: E[X])(using in: In[E, R]): Eff[R, X] = okay.one.Cont0.perform[E, R, X](op)
  /** `p` with its effect `E` handled by `h`, `E` taken off wherever it is in the row (the kernel's `handle`, an
   * extension Scala 2 cannot read) */
  def handle[E[_], R <: Row, T <: Row, A, B](p: Eff[R, A])(h: okay.one.handler.Handler[E, T, A, B])(using rm: Removed[E, R, T]): Eff[T, B] =
    okay.one.Cont0.handle(p)(h)
  /** a program with nothing left to handle, run */
  def run[A](p: Eff[Pure, A]): A = okay.one.Cont0.run(p)
  given here[E[_], T <: Row]: In[E, E +: T] = okay.one.row.In.Here()
  given there[E[_], F[_], T <: Row](using in: In[E, T]): In[E, F +: T] = okay.one.row.In.There(in)

/** an effect of two parameters, its first given: `Reader % Int` is the effect `[X] =>> Reader[Int, X]` */
infix type %[F[_, _], S] = okay.one.std.%[F, S]
/** THE PATH TO `E` IN `R`, as the facade finds it — from the place first: in a program needing effects, the place's
 * row (`Needs`), whatever row `R` is still unknown to be; at a handler's body or the top, the place's (`Place`); else
 * from the row, the kernel's `In`. So an operation bound first in a chain, `get.flatMap(f).flatMap(g)`, is of the
 * program's row, not of one the compiler made up for it */
final class Path[E[_], R <: Row](val in: In[E, R]) extends AnyVal
object Path extends PathInRow:
  /** in a program needing effects: the place's row, the path found among its effects — whatever `R` is still
   * unknown to be, so an operation bound first in a chain is of the program's row */
  given fromNeeds[E[_], Es[_]](using n: Needs[Es], r: Req[E, Es]): Path[E, n.R] = Path(n.paths.find(r))
sealed trait PathInRow extends PathGiven:
  /** in a row named: the kernel's path, walked here — the head, or past it */
  given here[E[_], T <: Row]: Path[E, E +: T] = Path(In.Here())
  given there[E[_], F[_], T <: Row](using p: Path[E, T]): Path[E, F +: T] = Path(In.There(p.in))
sealed trait PathGiven:
  /** the kernel's own evidence, given by hand or by a signature */
  given fromIn[E[_], R <: Row](using in: In[E, R]): Path[E, R] = Path(in)

/** `E :? R`, the effect `E` in the row `R`: a signature's — `(using Reader % E :? R)` */
infix type :?[E[_], R <: Row] = Path[E, R]
/** `E` in the row, as a bound: `def p[R <: Row: Has[Ask]: Has[Log]]: Eff[R, Int]` */
type Has[E[_]] = [R <: Row] =>> Path[E, R]

/** EFFECTS JOINED, `E1 + E2 + …`: what a program needs, in any order — `Reader % Int + Writer % String`. Two joined are
 * an effect again, a marker, so `+` goes on */
sealed trait Both[F[_], G[_], X]
infix type +[F[_], G[_]] = [X] =>> Both[F, G, X]

/** `E` AMONG THE EFFECTS JOINED `Es`: the one it is, or in the left or the right of two — the compiler finds it, the
 * left first */
enum Req[E[_], Es[_]]:
  case Only[E[_]]() extends Req[E, E]
  case Left[E[_], F[_], G[_]](r: Req[E, F]) extends Req[E, F + G]
  case Right[E[_], F[_], G[_]](r: Req[E, G]) extends Req[E, F + G]
object Req extends ReqLow:
  given only[E[_]]: Req[E, E] = Only()
  given left[E[_], F[_], G[_]](using r: Req[E, F]): Req[E, F + G] = Left(r)
sealed trait ReqLow:
  given right[E[_], F[_], G[_]](using r: Req[E, G]): Req[E, F + G] = Req.Right(r)

/** THE PATHS of the effects a program needs, `Es`, in the row `R`: one effect's, or of two joined each side's — the
 * compiler builds them where the program is placed (`Needs`); an effect among them found by its `Req` */
sealed abstract class Paths[Es[_], R <: Row]:
  def find[E[_]](r: Req[E, Es]): In[E, R]
object Paths extends PathsLow:
  /** one effect, `E0`, at its path. `Only` is its one case: `E0` an effect, not two joined — those have `Both`, by
   * priority — which the checker cannot tell apart, hence unchecked */
  final class One[E0[_], R <: Row](val head: In[E0, R]) extends Paths[E0, R]:
    def find[E[_]](r: Req[E, E0]): In[E, R] = (r: @unchecked) match
      case Req.Only() => head
  /** two effects joined, `F + G`, each side's paths found on its own. `Left`, `Right` its cases: an operation of
   * the marker `Both` itself there is none */
  final class Both[F[_], G[_], R <: Row](val left: Paths[F, R], val right: Paths[G, R]) extends Paths[F + G, R]:
    def find[E[_]](r: Req[E, F + G]): In[E, R] = (r: @unchecked) match
      case Req.Left(l) => left.find(l)
      case Req.Right(g) => right.find(g)
  given both[F[_], G[_], R <: Row](using left: Paths[F, R], right: Paths[G, R]): Paths[F + G, R] = Both(left, right)
sealed trait PathsLow:
  given one[E0[_], R <: Row](using head: In[E0, R]): Paths[E0, R] = Paths.One(head)

/** A PLACE WITH THE EFFECTS `Es`: a row that has them, and their paths in it — where a program needing them runs,
 * `A ! Es`. Made where the program is fitted to a row — a handler's body, a program over a row named — or in a
 * program's own place with more effects. One object a fitting, the facade's cost — the kernel knows nothing of it */
sealed abstract class Needs[Es[_]]:
  type R <: Row
  def paths: Paths[Es, R]
object Needs extends NeedsMore:
  def of[Es[_], R0 <: Row](p: Paths[Es, R0]): Needs[Es] { type R = R0 } = new Needs[Es]:
    type R = R0
    def paths: Paths[Es, R0] = p
  /** at a place: its row, the paths of these in it — the nearest. The place says the row: a handler's body, the top
   * (`run`), or one named by hand, `Place.at[R]` */
  given under[Es[_], R0 <: Row](using h: Place[R0], p: Paths[Es, R0]): (Needs[Es] { type R = R0 }) = of(p)
  /** an effect needed: its path in the place's row — what `perform` finds in a program needing effects */
  given found[E[_], Es[_]](using n: Needs[Es], r: Req[E, Es]): In[E, n.R] = n.paths.find(r)
sealed trait NeedsMore:
  /** in a program's place with more effects: its row, the paths of these in it */
  given more[Es[_], Fs[_]](using c: Needs[Fs], p: Paths[Es, c.R]): (Needs[Es] { type R = c.R }) = Needs.of(p)

/** WHERE A PROGRAM NEEDING EFFECTS IS FITTED TO A ROW — the row named: a handler's body, its effect in front of the
 * row outside; the top; one named by hand. So the handlers make the row, the outermost last, each its own at the
 * head. A row's place carries the row into the search for the program's paths */
opaque type Place[R <: Row] = Unit
object Place:
  /** the place of a handler of `E`, the row outside `T` — nothing made */
  inline def head[E[_], T <: Row]: Place[E +: T] = ()
  /** the top: no handler, no effect */
  inline def top: Place[Pure] = ()
  /** a row named by hand: `given Place[R] = Place.at` fits programs needing effects to `R` */
  inline def at[R <: Row]: Place[R] = ()

/** `A ! E1 + E2`: A PROGRAM OF `A` NEEDING THE EFFECTS `E1`, `E2` — in any order, in any row that has them among
 * others, under any handlers: a function of the place it runs at, its row there. `A ! Reader % Int`,
 * `A ! Choose + State % Int`. A program over a row named is `Eff[R, A]` */
infix type ![+A, Es[_]] = (c: Needs[Es]) ?=> Eff[c.R, A]

/** a program's body at a handler's place: over `E +: T`, the handler's effect at the head — what a handler takes; a
 * program needing effects is fitted to it; built when the run enters the handler */
type Under[E[_], T <: Row, A] = Place[E +: T] ?=> Eff[E +: T, A]

/** the effect of the shifts of answer `S`, the row outside their `reset` `T`: control as an effect of the row */
type Shifts[S, T <: Row] = okay.one.std.Shifts[S, T]
