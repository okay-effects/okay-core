package okay.core

import okay.std.*

/** OKAY'S INTERFACE, as okay's root `Effects[M]` has it (okay/src/main/scala/Effects.scala): a program `M[R, A]`, the
 * row in its type; an operation by its path; a handler taking its effect off wherever it is in the row. Here over
 * okay-core's rows and its `Program`, on the monad of a top — the shape okay would stand on */
trait OkayEffects[M[_ <: Row, _]]:
  def pure[R <: Row, A](a: A): M[R, A]
  def perform[E[_], R <: Row, X](op: E[X])(using In[E, R]): M[R, X]
  def defer[R <: Row, A, B](thunk: () => M[R, A])(f: A => M[R, B]): M[R, B]
  def tailcall[R <: Row, A](thunk: => M[R, A]): M[R, A] = defer(() => thunk)(pure)
  extension [R <: Row, A](m: M[R, A])
    def flatMap[B](f: A => M[R, B]): M[R, B]
    def map[B](f: A => B): M[R, B] = m.flatMap(a => pure(f(a)))
  def handle[E[_], A, Ans, R <: Row](h: Program.Handler[E, A, Ans])(m: M[R, A])(using rm: Removed[E, R]): M[rm.Out, Ans]
  def run[A](m: M[Pure, A]): A

/** okay's interface over `Program`, run on `top`'s monad */
final class OnProgram(top: Top) extends OkayEffects[Program]:
  def pure[R <: Row, A](a: A): Program[R, A] = Program.pure(a)
  def perform[E[_], R <: Row, X](op: E[X])(using in: In[E, R]): Program[R, X] = Program.perform(op)
  def defer[R <: Row, A, B](thunk: () => Program[R, A])(f: A => Program[R, B]): Program[R, B] = Program.defer(thunk()).flatMap(f)
  extension [R <: Row, A](m: Program[R, A])
    def flatMap[B](f: A => Program[R, B]): Program[R, B] = m.flatMap(f)
  def handle[E[_], A, Ans, R <: Row](h: Program.Handler[E, A, Ans])(m: Program[R, A])(using rm: Removed[E, R]): Program[rm.Out, Ans] =
    Program.handle(h)(m)
  def run[A](m: Program[Pure, A]): A = Program.run(top)(m)

/** a handler of an effect wherever it is in a row: okay's shape, on each monad */
abstract class TestAnywhereOn(top: Top, stackSafe: Boolean = true) extends munit.FunSuite:
  override def munitTestTransforms: List[TestTransform] = super.munitTestTransforms :+ TestTransform("stack", t =>
    if stackSafe then t else t.withBody(() => BackendSuite.onLargeStack(t.body())))

  // -- okay's TestEffectsRows, as it is written there
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]

  def reader[A](n: Int): Program.Handler[Ask, A, A] = new Program.Handler[Ask, A, A]:
    def ret(a: A): A = a
    def apply[R <: Row, X](ops: Program.Ops[R])(op: Ask[X], k: X => ops.P[A]): ops.P[A] = op match
      case Ask.Number => k(n)
  def writer[A]: Program.Handler[Say, A, (List[String], A)] = new Program.Handler[Say, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[R <: Row, X](ops: Program.Ops[R])(op: Say[X], k: X => ops.P[(List[String], A)]): ops.P[(List[String], A)] = op match
      case Say.Line(s) => ops.map(k(()))((log, a) => (s :: log, a))

  /** a program written once over the interface: its row declared, each operation by its path */
  def prog[M[_ <: Row, _]](using E: OkayEffects[M]): M[Ask +: Say +: Pure, Int] =
    E.perform[Ask, Ask +: Say +: Pure, Int](Ask.Number).flatMap(n =>
      E.perform[Say, Ask +: Say +: Pure, Unit](Say.Line(n.toString)).map(_ => n + 1))

  given E: OkayEffects[Program] = OnProgram(top)

  test("okay's interface over Program: handlers apply in any order, each takes its effect wherever it is"):
    assertEquals(E.run(E.handle(writer[Int])(E.handle(reader[Int](41))(prog[Program]))), (List("41"), 42))
    assertEquals(E.run(E.handle(reader[(List[String], Int)](41))(E.handle(writer[Int])(prog[Program]))), (List("41"), 42))

  test("okay's interface: a tail call costs no frame — 100 000 of them"):
    def loop[M[_ <: Row, _]](n: Int, acc: Int)(using E: OkayEffects[M]): M[Ask +: Pure, Int] =
      if n == 0 then E.pure(acc)
      else E.perform[Ask, Ask +: Pure, Int](Ask.Number).flatMap(x => E.tailcall(loop[M](n - 1, acc + x)))
    assertEquals(E.run(E.handle(reader[Int](1))(loop[Program](100000, 0))), 100000)

  // -- the standard effects taken off wherever they are
  type Row3 = State % Int +: Writer % String +: Throws % String +: Pure
  def counting: Program[Row3, Int] =
    Programs.get[Row3, Int].flatMap(n => Programs.put[Row3, Int](n + 1).flatMap(_ => Programs.tell[Row3, String](s"$n")
      .flatMap(_ => if n > 100 then Programs.raise[Row3, String, Int]("big") else Programs.get[Row3, Int])))

  test("the standard handlers in any order: the order of handlers is the semantics, the row's says nothing"):
    assertEquals(Program.run(top)(Programs.throws[String](Programs.writer[String](Programs.state(41)(counting)))),
      Right((List("41"), (42, 42))))
    assertEquals(Program.run(top)(Programs.state(41)(Programs.throws[String](Programs.writer[String](counting)))),
      (42, Right((List("41"), 42))))
    assertEquals(Program.run(top)(Programs.writer[String](Programs.throws[String](Programs.state(41)(counting)))),
      (List("41"), Right((42, 42))))
    // a failure under the state keeps it, over the state loses it
    assertEquals(Program.run(top)(Programs.state(200)(Programs.writer[String](Programs.throws[String](counting)))), (201, (List("200"), Left("big"))))
    assertEquals(Program.run(top)(Programs.writer[String](Programs.throws[String](Programs.state(200)(counting)))), (List("200"), Left("big")))

  test("a handler inside a body taken off by an outer one: paths translated through both"):
    type Inner = Reader % Int +: State % Int +: Pure
    val body: Program[Inner, Int] = Programs.ask[Inner, Int].flatMap(a => Programs.get[Inner, Int].map(_ + a))
    // the state outside, handled first though it is second in the row; the reader inside, after it
    val p = Programs.reader(5)(Programs.state(10)(body).map(_._2))
    assertEquals(Program.run(top)(p), 15)

  test("local and handleWith inside a body whose row was another's: the interceptor's path translated"):
    type R2 = Throws % String +: Reader % Int +: Pure
    val body: Program[R2, Int] = Programs.local[R2, Int, Int](_ * 10)(Programs.ask[R2, Int])
      .flatMap(a => Programs.handleWith[R2, String, Int](Programs.raise[R2, String, Int]("x"))(e => Program.pure(e.length + a)))
    assertEquals(Program.run(top)(Programs.throws[String](Programs.reader(4)(body))), Right(41))

  // -- one's own effect written once, in context; its Program handler derived
  enum Log[+A]:
    case Line(s: String) extends Log[Unit]
  def logging[A](using c: Effects)(body: c.Body[Log, (List[String], A), A]): c.Prog[(List[String], A)] =
    Effects.handle(new Handler[c.Prog, Log, A, (List[String], A)]:
      def ret(a: A): (List[String], A) = (Nil, a)
      def apply[X](op: Log[X], k: X => c.Prog[(List[String], A)]): c.Prog[(List[String], A)] = op match
        case Log.Line(l) => k(()).map((ls, a) => (l :: ls, a)))(body)
  def loggingP[R <: Row, A](p: Program[R, A])(using rm: Removed[Log, R]) =
    Program.handled[Log, R, A, (List[String], A)](p)(c => b => logging[A](using c)(b))

  test("one's own effect written in context, its Program handler one line: taken off wherever it is"):
    type R5 = Reader % Int +: Log +: Pure
    val p: Program[R5, Int] = Programs.ask[R5, Int].flatMap(n => Program.perform[Log, R5, Unit](Log.Line(s"n=$n")).map(_ => n * 2))
    assertEquals(Program.run(top)(Programs.reader(21)(loggingP(p))), (List("n=21"), 42))

  test("100 000 operations of an effect deep in the row, its handler taken off from there"):
    type R4 = Writer % Int +: Reader % Int +: State % Int +: Pure
    def loop(i: Int): Program[R4, Unit] =
      if i == 0 then Program.pure(()) else Programs.get[R4, Int].flatMap(s => Programs.put[R4, Int](s + 1)).flatMap(_ => loop(i - 1))
    assertEquals(Program.run(top)(Programs.reader(0)(Programs.writer[Int](Programs.state(0)(loop(100000)))))._1, Nil)
    assertEquals(Program.run(top)(Programs.reader(0)(Programs.writer[Int](Programs.state(0)(loop(100000)))))._2._1, 100000)

class TestAnywhere extends TestAnywhereOn(okay.cont.top)
class TestAnywhereOnCont0 extends TestAnywhereOn(okay.zero.top)
class TestAnywhereOnRef extends TestAnywhereOn(okay.zero.Ref.top)
/** the textbook CPS, not stack safe: on a thread of a large stack */
class TestAnywhereOnCps0 extends TestAnywhereOn(Cps0.top, stackSafe = false)
