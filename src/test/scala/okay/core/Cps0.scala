package okay.core

/** A TEXTBOOK REFERENCE of `Effects`, no code of `Cont` nor of `Cont0`: shift0 and `$` in CPS — a program a function
 * `(A => I) => O`, `shift0 f` the function `f` itself, `ret $ body` the body applied to `ret` (Materzok–Biernacki).
 * Under a delimiter the answers are programs of the level outside, yielding the body's answer or a request; an
 * operation and a `shift` requests, a handler and a reset a `$` whose answers are read — `okay.zero.Ref`'s reading,
 * on functions. Not stack safe: for small programs only. Master's semantics: a shift's `k` under a handler the rest
 * under the handler's `ret`, its operations the shift body's handler's */
object Cps0:
  /** `(A => I) => O`, as a class */
  final class F[I, O, A](val run: (A => I) => O)
  def pure[S, A](a: A): F[S, S, A] = F(k => k(a))
  extension [I, O, A](m: F[I, O, A])
    def flatMap[I2, B](f: A => F[I2, I, B]): F[I2, O, B] = F(k => m.run(a => f(a).run(k)))
    def map[B](f: A => B): F[I, O, B] = F(k => m.run(a => k(f(a))))
  /** `shift0 f`: `f` — the rest given to it as it is */
  def shift0[I, O, A](f: (A => I) => O): F[I, O, A] = F(f)
  /** `ret $ body`: the body applied to `ret` */
  def dollar[I, O, A](body: F[I, O, A], ret: A => I): O = body.run(ret)

  /** what a body answers its delimiter: its answer; an operation of its row with the rest; a `shift` with its body
   * and the rest — the rests programs of the level outside */
  sealed trait Res[R <: Row, J, V]
  final case class Done[R <: Row, J, V](v: V) extends Res[R, J, V]
  final case class Request[R <: Row, J, V, E[_], X](op: E[X], in: In[E, R], k: X => Lv[R, J, V]) extends Res[R, J, V]
  final case class Ctl[R <: Row, J, V, A, I](f: (A => Eff[R, J, V, V, I]) => Eff[R, J, V, V, V], k: A => Lv[R, J, I]) extends Res[R, J, V]
  /** a level: a program outside, at `J`, yielding the body's answer `V` or a request */
  type Lv[R <: Row, J, V] = F[J, J, Res[R, J, V]]
  /** a program over `R` under a delimiter whose outside is at `J` */
  type Eff[R <: Row, J, I, O, A] = F[Lv[R, J, I], Lv[R, J, O], A]

  /** an operation: `shift0(k => Request(op, k))` */
  def request[E[_], R <: Row, J, T, X](op: E[X], in: In[E, R]): Eff[R, J, T, T, X] = shift0(k => pure(Request(op, in, k)))
  /** a `shift`: `shift0(k => Ctl(f, k))` */
  def control[R <: Row, J, A, I, O](f: (A => Eff[R, J, O, O, I]) => Eff[R, J, O, O, O]): Eff[R, J, I, O, A] =
    shift0(k => pure(Ctl(f, k)))

  /** a delimiter's reading of its body's answers: its answer; an operation by `on`; a shift's body under it again */
  def read[RB <: Row, R <: Row, J, T, V](out: => Eff[R, J, T, T, Res[RB, Lv[R, J, T], V]],
      on: [E[_], X] => (E[X], In[E, RB], X => Eff[R, J, T, T, Res[RB, Lv[R, J, T], V]]) => Eff[R, J, T, T, V]): Eff[R, J, T, T, V] =
    // `out` made when run: a rest resumed — `k(x)` — runs nothing by being made
    F(kk => out.flatMap(read1(on)).run(kk))
  private def read1[RB <: Row, R <: Row, J, T, V](
      on: [E[_], X] => (E[X], In[E, RB], X => Eff[R, J, T, T, Res[RB, Lv[R, J, T], V]]) => Eff[R, J, T, T, V])
      : Res[RB, Lv[R, J, T], V] => Eff[R, J, T, T, V] = res => res match
    case Done(v) => pure(v)
    case r: Request[rb, j, v, e, x] => on[e, x](r.op, r.in, r.k)
    case c: Ctl[rb, j, v, a, i] => read(again(c), on)

  /** a handler's reading: its own by the clause, the rest read again — deep; another's asked again outside */
  def readHand[E[_], R <: Row, J, T, Ans](out: => Eff[R, J, T, T, Res[E +: R, Lv[R, J, T], Ans]],
      h: Handler[[X] =>> Eff[R, J, T, T, X], E, ?, Ans]): Eff[R, J, T, T, Ans] =
    read[E +: R, R, J, T, Ans](out, [E2[_], X] => (op: E2[X], in: In[E2, E +: R], k: X => Eff[R, J, T, T, Res[E +: R, Lv[R, J, T], Ans]]) =>
      in match
        case In.Here() => h(op, x => readHand(k(x), h))
        case In.There(o) => request[E2, R, J, T, X](op, o).flatMap(x => readHand(k(x), h)))

  /** an interceptor's reading: an operation at its path by the clause, the rest read again; another asked again
   * outside, its path the same */
  def readIntercept[E[_], R <: Row, J, T, Ans](target: In[E, R], out: => Eff[R, J, T, T, Res[R, Lv[R, J, T], Ans]],
      h: Handler[[X] =>> Eff[R, J, T, T, X], E, ?, Ans]): Eff[R, J, T, T, Ans] =
    read[R, R, J, T, Ans](out, [E2[_], X] => (op: E2[X], in: In[E2, R], k: X => Eff[R, J, T, T, Res[R, Lv[R, J, T], Ans]]) =>
      In.same(in, target) match
        case null => request[E2, R, J, T, X](op, in).flatMap(x => readIntercept(target, k(x), h))
        case same => h(same(op), x => readIntercept(target, k(x), h)))

  /** a reset's reading: every operation asked again outside */
  def passAll[R <: Row, J, T, O](out: => Eff[R, J, T, T, Res[R, Lv[R, J, T], O]]): Eff[R, J, T, T, O] =
    read[R, R, J, T, O](out, [E[_], X] => (op: E[X], in: In[E, R], k: X => Eff[R, J, T, T, Res[R, Lv[R, J, T], O]]) =>
      request[E, R, J, T, X](op, in).flatMap(x => passAll(k(x))))

  /** a shift's body under its delimiter again, `k` the rest resumed inside */
  def again[RB <: Row, J, V, A, I](c: Ctl[RB, J, V, A, I]): Lv[RB, J, V] =
    dollar(c.f(a => inside(c.k(a))), (v: V) => pure(Done[RB, J, V](v)))

  /** a rest, a program outside, resumed from inside: each operation it asks made again inside */
  def inside[RB <: Row, J, V, I](s: => Lv[RB, J, I]): Eff[RB, J, V, V, I] =
    shift0[Lv[RB, J, V], Lv[RB, J, V], Res[RB, J, I]](kk => s.flatMap(kk)).flatMap:
      case Done(i) => pure(i)
      case r: Request[rb, j, i, e, x] => request[e, RB, J, V, x](r.op, r.in).flatMap(x => inside[RB, J, V, I](r.k(x)))
      case c: Ctl[rb, j, i, a, i2] => inside[RB, J, V, I](again(c))

  /** the context: the row `R0`, the outside of its delimiter at `J0`, the answer `T0` */
  class At[R0 <: Row, J0, T0] extends Effects:
    type M[I, O, A] = Eff[R0, J0, I, O, A]
    type Within[I, O, A] = Eff[R0, Lv[R0, J0, T0], I, O, A]
    type Handling[E[_], I, O, A] = Eff[E +: R0, Lv[R0, J0, T0], I, O, A]
    type Effs = R0
    type S = T0
    type Handled[E[_], S2] = At[E +: R0, Lv[R0, J0, T0], S2]
    type Reset[S2] = At[R0, Lv[R0, J0, T0], S2]
    type Answer[S2] = At[R0, J0, S2]
    def answer[S2]: At[R0, J0, S2] = At()
    def top: Top = Cps0.top
    def pure[S2, A](a: A): M[S2, S2, A] = Cps0.pure(a)
    def delay[I, O, A](m: => M[I, O, A]): M[I, O, A] = F(k => m.run(k))
    extension [I, O, A](m: M[I, O, A])
      def flatMap[I2, B](f: A => M[I2, I, B]): M[I2, O, B] = Cps0.flatMap(m)(f)
      def map[B](f: A => B): M[I, O, B] = Cps0.map(m)(f)
    def perform[E[_], S2, X](op: E[X], in: In[E, R0]): M[S2, S2, X] = request(op, in)
    def handle[E[_], A, Ans](h: Handler[Prog, E, A, Ans])(body: At[E +: R0, Lv[R0, J0, T0], Ans] ?=> Handling[E, Ans, Ans, A]): M[T0, T0, Ans] =
      readHand(dollar(body(using At()), (a: A) => Cps0.pure(Done[E +: R0, Lv[R0, J0, T0], Ans](h.ret(a)))), h)
    def intercept[E[_], A, Ans](in: In[E, R0], h: Handler[Prog, E, A, Ans])(body: At[R0, Lv[R0, J0, T0], Ans] ?=> Within[Ans, Ans, A]): M[T0, T0, Ans] =
      readIntercept(in, dollar(body(using At()), (a: A) => Cps0.pure(Done[R0, Lv[R0, J0, T0], Ans](h.ret(a)))), h)
    def shift[I, O, A](f: (A => M[O, O, I]) => M[O, O, O], same: (I =:= O) | Null): M[I, O, A] = control(f)
    def reset[S2, O](body: At[R0, Lv[R0, J0, T0], S2] ?=> Within[S2, O, S2]): M[T0, T0, O] =
      passAll(dollar(body(using At()), (s: S2) => Cps0.pure(Done[R0, Lv[R0, J0, T0], S2](s))))
    type Needing[Es <: AnyKind] = At[R0, J0, T0] & Needs[Es]
    def needing[Es <: AnyKind](p: Paths[Es, R0]): At[R0, J0, T0] & Needs[Es] = Needy(p)

  final class Needy[R0 <: Row, J0, T0, Es <: AnyKind](p: Paths[Es, R0]) extends At[R0, J0, T0] with Needs[Es]:
    def find[E[_]](r: Req[E, Es]): In[E, R0] = p.find(r)

  /** the top: a delimiter of its own, its answer the value; a shift up to it the body's */
  def run[A](body: At[Pure, A, A] ?=> Eff[Pure, A, A, A, A]): A =
    def top(out: F[A, A, Res[Pure, A, A]]): F[A, A, A] = out.flatMap:
      case Done(a) => pure(a)
      case c: Ctl[r, j, v, b, i] => top(again(c))
    top(dollar(body(using At()), (a: A) => pure(Done[Pure, A, A](a)))).run(identity)

  /** the textbook's top, for a library re-entering it */
  object top extends Top:
    type M[J, I, O, A] = Eff[Pure, J, I, O, A]
    def at[J]: At[Pure, J, J] = At()
    def run[J](p: Eff[Pure, J, J, J, J]): J = Cps0.run(p)
