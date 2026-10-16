package okay.core

import okay.one.{Cont, Delim}
import scala.util.NotGiven
import okay.one.row.{+:, Control, In, Nearest, Pure, Row}

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
type Eff[R <: Row, +A] = okay.one.Cont[R, Nothing, Nothing, A]   // `okay.one`'s `A ! R`, written out: Scala 2 sees no top-level alias

/** THE KERNEL'S WORDS FOR SCALA 2, which cannot read an inline definition (the kernel's `perform`, its `In.here`):
 * `perform` by a path given, and the paths as implicits — `import okay.core.Ops._` */
object Ops:
  def perform[E[_], R <: Row, X](op: E[X])(using in: In[E, R]): Eff[R, X] = okay.one.Cont.perform[E, R, X](op)
  /** `p` with its effect `E` handled by `h`, `E` taken off wherever it is in the row (the kernel's `handle`, an
   * extension Scala 2 cannot read) */
  def handle[E[_], R <: Row, T <: Row, A, B](p: Eff[R, A])(h: okay.one.handler.Handler[E, T, A, B])(using rm: Removed[E, R, T]): Eff[T, B] =
    okay.one.Cont.handle(p)(h)
  /** a program with nothing left to handle, run */
  def run[A](p: Eff[Pure, A]): A = okay.one.Cont.run(p)
  /** the binds, which the kernel inlines */
  def flatMap[R <: Row, A, B](p: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] = p.flatMap(f)
  def map[R <: Row, A, B](p: Eff[R, A])(f: A => B): Eff[R, B] = p.map(f)
  given here[E[_], T <: Row]: In[E, E +: T] = Paths.head[E, T]
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
  given here[E[_], T <: Row]: Path[E, E +: T] = Path(Paths.head[E, T])
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

/** NO EFFECTS: what a program needs that needs nothing — the rest when a runner took a program's only effect */
sealed trait NoEffects[X]

/** `E` TAKEN FROM THE EFFECTS `Es`, wherever it is among them, in any order — what is left, `Rest`, and how the paths
 * of the rest in a row `T` with `E` put at its head are the paths of `Es` there. Found where a runner is called,
 * from the program's own effects: a runner `def run[Es[_]](p: A ! Es)(using s: Split[Es, E]): B ! s.Rest` takes
 * `E` off a program whatever the order its effects were written in — what a union gave, with a path for every
 * operation. Over a rest named abstractly, `E + F`, it is `F` itself (`head`) */
sealed abstract class Split[Es[_], E[_]]:
  type Rest[_]
  def paths[T <: Row](rest: Paths[Rest, T]): Paths[Es, E +: T]
  /** `p` fitted under a handler of `E` at the head of the rest's place: a runner's body is `handler(s.fit(p))` */
  def fit[A](p: A ! Es)(using c: Needs[Rest]): Eff[E +: c.R, A] = p(using Needs.of(paths(c.paths)))
object Split extends SplitEdge:
  private[core] type K[x] = Any
  private object Whole extends Split[K, K]:
    type Rest[x] = NoEffects[x]
    def paths[T <: Row](rest: Paths[NoEffects, T]): Paths[K, K +: T] = Paths.of(Paths.head[K, T])
  /** `Es` is `E` alone: nothing left — one shared, it holds nothing */
  given whole[E[_]]: (Split[E, E] { type Rest[x] = NoEffects[x] }) =
    Whole.asInstanceOf[Split[E, E] { type Rest[x] = NoEffects[x] }]
sealed trait SplitEdge extends SplitEdgeLast:
  private object Head extends Split[Split.K, Split.K]:
    type Rest[x] = Any
    def paths[T <: Row](rest: Paths[Split.K, T]): Paths[Split.K, Split.K +: T] =
      Paths.Both(Paths.One(In.Here()), rest.under[Split.K]).asInstanceOf[Paths[Split.K, Split.K +: T]]
  /** `E` the left of two: the rest the right — one shared */
  given head[E[_], G[_]]: (Split[E + G, E] { type Rest[x] = G[x] }) = Head.asInstanceOf[Split[E + G, E] { type Rest[x] = G[x] }]
/** `E` the right of two, below `head`: of `E + E` the left is taken — the nearer place, as a handler of `E` takes
 * the nearer operation */
sealed trait SplitEdgeLast extends SplitDeep:
  private object Last extends Split[Split.K, Split.K]:
    type Rest[x] = Any
    def paths[T <: Row](rest: Paths[Split.K, T]): Paths[Split.K, Split.K +: T] =
      Paths.Both(rest.under[Split.K], Paths.One(In.Here())).asInstanceOf[Paths[Split.K, Split.K +: T]]
  /** `E` the right of two: the rest the left — one shared */
  given last[F[_], E[_]]: (Split[F + E, E] { type Rest[x] = F[x] }) = Last.asInstanceOf[Split[F + E, E] { type Rest[x] = F[x] }]
sealed trait SplitDeep extends SplitDeepRight:
  /** `E` deeper in the left: the rest the left's rest joined to the right */
  given left[F[_], G[_], E[_]](using s: Split[F, E]): (Split[F + G, E] { type Rest[x] = Both[s.Rest, G, x] }) =
    new Split[F + G, E]:
      type Rest[x] = Both[s.Rest, G, x]
      def paths[T <: Row](rest: Paths[s.Rest + G, T]): Paths[F + G, E +: T] = (rest: @unchecked) match
        case b: Paths.Both[l, r, t] => Paths.Both(s.paths(b.left), b.right.under[E])
sealed trait SplitDeepRight:
  /** `E` deeper in the right: the left joined to the right's rest */
  given right[F[_], G[_], E[_]](using s: Split[G, E]): (Split[F + G, E] { type Rest[x] = Both[F, s.Rest, x] }) =
    new Split[F + G, E]:
      type Rest[x] = Both[F, s.Rest, x]
      def paths[T <: Row](rest: Paths[F + s.Rest, T]): Paths[F + G, E +: T] = (rest: @unchecked) match
        case b: Paths.Both[l, r, t] => Paths.Both(b.left.under[E], s.paths(b.right))

/** THE PATHS of the effects a program needs, `Es`, in the row `R`: one effect's, or of two joined each side's — the
 * compiler builds them where the program is placed (`Needs`); an effect among them found by its `Req` */
sealed abstract class Paths[Es[_], R <: Row]:
  def find[E[_]](r: Req[E, Es]): In[E, R]
  /** the same paths one deeper, under `E` put at the head of `R` — a place made from a place */
  def under[E[_]]: Paths[Es, E +: R]
  /** the paths of `E` among these: the whole, or a side of two joined — a place made from a place, no search for
   * the effects a program names abstractly, `F`: a search would build `One` for what is a sum at the call */
  def sub[E[_]](r: Req[E, Es]): Paths[E, R]
object Paths extends PathsRest:
  /** THE HEAD'S PATH, ONE SHARED (small-runs): a given made a new one each operation, and so did `one` around it —
   * two objects an operation for nothing. It holds no effect: its look-up cache (`In.cache`) is keyed by the level,
   * where a row's head is one effect, so one path serves every head; a race costs a look-up again, as the cache says */
  private type AnyE[x] = Any
  private val anyHead: In[AnyE, AnyE +: Pure] = In.Here[AnyE, Pure]()
  private val anyHeadOne: One[AnyE, AnyE +: Pure] = One(anyHead)
  def head[E[_], T <: Row]: In[E, E +: T] = anyHead.asInstanceOf[In[E, E +: T]]
  /** `One(head)`, the head's shared when it is the head's */
  def of[E0[_], R <: Row](head: In[E0, R]): Paths[E0, R] =
    if head eq anyHead then anyHeadOne.asInstanceOf[Paths[E0, R]] else One(head)
  /** one effect, `E0`, at its path. `Only` is its one case: `E0` an effect, not two joined — those have `Both`, by
   * priority — which the checker cannot tell apart, hence unchecked */
  final class One[E0[_], R <: Row](val head: In[E0, R]) extends Paths[E0, R]:
    def find[E[_]](r: Req[E, E0]): In[E, R] = (r: @unchecked) match
      case Req.Only() => head
    def under[E[_]]: Paths[E0, E +: R] = One(In.There(head))
    def sub[E[_]](r: Req[E, E0]): Paths[E, R] = (r: @unchecked) match
      case Req.Only() => this
  /** two effects joined, `F + G`, each side's paths found on its own. `Left`, `Right` its cases: an operation of
   * the marker `Both` itself there is none */
  final class Both[F[_], G[_], R <: Row](val left: Paths[F, R], val right: Paths[G, R]) extends Paths[F + G, R]:
    def find[E[_]](r: Req[E, F + G]): In[E, R] = (r: @unchecked) match
      case Req.Left(l) => left.find(l)
      case Req.Right(g) => right.find(g)
    def under[E[_]]: Paths[F + G, E +: R] = Both(left.under[E], right.under[E])
    def sub[E[_]](r: Req[E, F + G]): Paths[E, R] = r match
      case Req.Only() => this
      case Req.Left(l) => left.sub(l)
      case Req.Right(g) => right.sub(g)
  given both[F[_], G[_], R <: Row](using left: Paths[F, R], right: Paths[G, R]): Paths[F + G, R] = Both(left, right)
  /** no effects: no path to find — `Req[E, NoEffects]` there is none */
  final class None[R <: Row] extends Paths[NoEffects, R]:
    def find[E[_]](r: Req[E, NoEffects]): In[E, R] = throw MatchError(r)
    def under[E[_]]: Paths[NoEffects, E +: R] = None()
    /** the whole, no effects, is itself; no effect is among none */
    def sub[E[_]](r: Req[E, NoEffects]): Paths[E, R] = (r: @unchecked) match
      case Req.Only() => this
  private val theNone: None[Pure] = None()
  /** one shared: no effects, no path */
  given none[R <: Row]: Paths[NoEffects, R] = theNone.asInstanceOf[Paths[NoEffects, R]]
sealed trait PathsRest extends PathsLow:
  /** at a place with the effects `E` themselves, `c`: its own paths — so a rest named abstractly, `F` in
   * `A ! Reader % R + F`, is fitted by the paths it came with, a sum at the call as it may be (an `In` for `F`
   * would be a place for a sum, `find` on it a `MatchError`); under one, two, three handlers inside — their effects
   * not `E`, which is `one`'s, `In.here`. Above `one`, below `both` */
  given rest[E[_]](using c: Needs[E]): Paths[E, c.R] = c.paths
  given rest1[E[_], H[_]](using c: Needs[E], n1: NotGiven[H[Any] =:= E[Any]]): Paths[E, H +: c.R] = c.paths.under[H]
  given rest2[E[_], H[_], H2[_]](using c: Needs[E], n1: NotGiven[H[Any] =:= E[Any]], n2: NotGiven[H2[Any] =:= E[Any]])
      : Paths[E, H +: H2 +: c.R] = c.paths.under[H2].under[H]
  given rest3[E[_], H[_], H2[_], H3[_]](using c: Needs[E], n1: NotGiven[H[Any] =:= E[Any]], n2: NotGiven[H2[Any] =:= E[Any]],
      n3: NotGiven[H3[Any] =:= E[Any]]): Paths[E, H +: H2 +: H3 +: c.R] = c.paths.under[H3].under[H2].under[H]
sealed trait PathsLow:
  given one[E0[_], R <: Row](using head: In[E0, R]): Paths[E0, R] = Paths.of(head)

/** A PLACE WITH THE EFFECTS `Es`: a row that has them, and their paths in it — where a program needing them runs,
 * `A ! Es`. Made where the program is fitted to a row — a handler's body, a program over a row named — or in a
 * program's own place with more effects. One object a fitting, the facade's cost — the kernel knows nothing of it */
sealed abstract class Needs[Es[_]]:
  type R <: Row
  def paths: Paths[Es, R]
object Needs extends NeedsPart:
  def of[Es[_], R0 <: Row](p: Paths[Es, R0]): Needs[Es] { type R = R0 } = new Needs[Es]:
    type R = R0
    def paths: Paths[Es, R0] = p
  /** at a place: its row, the paths of these in it — the nearest. The place says the row: a handler's body, the top
   * (`run`), or one named by hand, `Place.at[R]` */
  given under[Es[_], R0 <: Row](using h: Place[R0], p: Paths[Es, R0]): (Needs[Es] { type R = R0 }) = of(p)
  /** an effect needed: its path in the place's row — what `perform` finds in a program needing effects */
  given found[E[_], Es[_]](using n: Needs[Es], r: Req[E, Es]): In[E, n.R] = n.paths.find(r)
sealed trait NeedsPart extends NeedsMore:
  /** in a program's place whose effects have these among them, `Es` a part of `Fs`: the part's own paths — a rest
   * `F` among `Choose + F` needed alone is fitted by the paths it came with, not by a place for `F` as one effect */
  given part[Es[_], Fs[_]](using c: Needs[Fs], r: Req[Es, Fs]): (Needs[Es] { type R = c.R }) = Needs.of(c.paths.sub(r))
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

/** A PLACE UNDER A `$` WITH THE EFFECTS `Es`: its row `R`, and `T`, the row outside the nearest `$` in `R` — where a
 * program moving the answer runs, its answers programs over `T`. One object a fitting, as `Needs`; a `Needs` itself
 * (a `Needs` itself), so a program needing effects is bound in it as it is, lifted to the answers */
sealed abstract class Delimited[Es[_]] extends Needs[Es]:
  type T <: Row
  def at: Nearest[T, R]
object Delimited:
  /** under a `$` put over the place `c`: its paths one deeper, the `$` the nearest, `c`'s row outside it */
  def under[Es[_]](c: Needs[Es]): Delimited[Es] { type R = Delim[c.R]; type T = c.R } = new Delimited[Es]:
    type R = Delim[c.R]
    type T = c.R
    def paths: Paths[Es, Delim[c.R]] = c.paths.under[Control[c.R]]
    def at: Nearest[c.R, Delim[c.R]] = Nearest.Here()
  /** at a place with the effects, a `$` in its row: the nearest one, `T` outside it. A `Needs` itself, the nearer
   * and the more specific where a program of effects is bound inside: found before the place's outside the `$` */
  given of[Es[_], T0 <: Row](using c: Needs[Es], n: Nearest[T0, c.R]): (Delimited[Es] { type R = c.R; type T = T0 }) =
    new Delimited[Es]:
      type R = c.R
      type T = T0
      def paths: Paths[Es, c.R] = c.paths
      def at: Nearest[T0, c.R] = n

/** `Atm[A, X, Y, Es]`: A PROGRAM OF `A` NEEDING THE EFFECTS `Es`, UNDER A `$`, ITS ANSWER MOVED — from `X`, the
 * rest's up to the `$`, to `Y`, the `$`'s: shapes over the row outside the `$`, `T` — `Hole[X, A]` an answer holding
 * `k` (printf, a state in the answer), `Fixed[A]` one as it is, any `[T <: Row] =>> …`. No row named, as
 * `A ! Es`; the `$` and the handlers inside it make the row: `Atm.dollar(handleAt(Reader(5))(body))(ret)`.
 * `Atm[String, Fixed[String], Hole[Int, String], Reader % Int]`. Not `A ! Es` with the `$` among `Es`: `Y` has to be a
 * constructor, over the row outside, and a match type on `Es` cannot give one back — nor may the alias match on
 * `c.T`, which the compiler stands in for while it checks the body, and a match on a stand-in is not reduced */
type Atm[+A, X[_ <: Row], Y[_ <: Row], Es[_]] = (c: Delimited[Es]) ?=> Cont[c.R, Eff[c.T, X[c.T]], Eff[c.T, Y[c.T]], A]
/** a `$`'s answer: a hole of `X` in a program of `A` over the row outside — printf's `Int => Eff[T, String]` */
type Hole[X, A] = [T <: Row] =>> X => Eff[T, A]
/** a `$`'s answer: `A`, the rest's as it is */
type Fixed[A] = [T <: Row] =>> A

/** a program's body at a handler's place: over `E +: T`, the handler's effect at the head — what a handler takes; a
 * program needing effects is fitted to it; built when the run enters the handler */
type Under[E[_], T <: Row, A] = Place[E +: T] ?=> Eff[E +: T, A]
/** the same at any answers: the body a program moving the answer, under a handler leaving the rest (`handleAt`) */
type UnderAt[E[_], T <: Row, I, O, A] = Place[E +: T] ?=> Cont[E +: T, I, O, A]

/** the effect of the shifts of answer `S`, the row outside their `reset` `T`: control as an effect of the row */
type Shifts[S, T <: Row] = okay.one.std.Shifts[S, T]
