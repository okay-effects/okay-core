package okay.zero

import okay.core.*
import okay.core.Control.{reset, shift}
import okay.std.*
import scala.util.Random

/** THE MONADS AGAINST THE REFERENCES ON PROGRAMS MADE AT RANDOM: handlers nested in any order, operations of the
 * effects in scope, resets and shifts to them — written once over the interface, run on `Cont`, on `Cont0`, on
 * `Ref` (the kernel alone) and on `Cps0` (the textbook CPS): the four agree. okay-java's AgreeTest.
 *
 * A program is data, made first, then built — so every run builds the same. The paths to the effects are made as
 * the handlers nest, at run time: a test's casts, not the library's. A shift is made only where its nearest
 * delimiter is a reset: up to a handler `Cont` and `Cont0` differ by design (`TestMonads`). A program they disagree
 * on is shrunk before it is shown: cut, a step at a time, to one no step smaller of which disagrees */
class TestAgree extends munit.FunSuite:

  enum Kind:
    case Write, St, Ch, Thr, Rd, Rst

  enum P:
    case Pure(n: Int)
    case Seq(a: P, b: P)
    /** an operation of the handler `index` out, `0` the innermost */
    case Op(index: Int, n: Int)
    case Handle(k: Kind, n: Int, body: P)

  /** a program over the scope, the kinds of the handlers in force, the innermost first */
  def make(r: Random, scope: List[Kind], budget: Int): P =
    val pick = if budget <= 1 then r.nextInt(2) else 2 + r.nextInt(4)
    if pick <= 2 then
      if scope.isEmpty || pick == 0 then P.Pure(r.nextInt(10))
      else
        val i = r.nextInt(scope.size)
        scope(i) match
          case Kind.Ch if r.nextInt(3) > 0 => P.Pure(r.nextInt(10)) // a choice multiplies the paths: rarely
          case Kind.Rst if i > 0 => P.Pure(r.nextInt(10))           // a shift only up to the nearest delimiter
          case _ => P.Op(i, r.nextInt(10))
    else if pick == 3 then P.Seq(make(r, scope, budget / 2), make(r, scope, budget / 2))
    else
      val k = Kind.values(r.nextInt(Kind.values.length))
      P.Handle(k, r.nextInt(10), make(r, k :: scope, budget - 1))

  /** the path past `i` effects of the row: `There` `i` times, then `Here` — typed by a cast, the row known only at
   * run time */
  def path(i: Int): In[?, ?] = if i == 0 then In.Here() else In.There(path(i - 1))

  def build(p: P, scope: List[Kind])(using c: Effects): c.Prog[Int] = p match
    case P.Pure(n) => c.pure(n)
    case P.Seq(a, b) => build(a, scope).flatMap(x => build(b, scope).map(y => x * 3 + y))
    case P.Op(i, n) =>
      // a reset adds no effect to the row: the path counts the handlers between, not the resets
      val in = path(scope.take(i).count(_ != Kind.Rst))
      scope(i) match
        case Kind.Write => tell[Int](n)(using c, in.asInstanceOf[Writer % Int :? c.Effs]).map(_ => n)
        case Kind.St =>
          given (State % Int :? c.Effs) = in.asInstanceOf[State % Int :? c.Effs]
          modify[Int](_ + n).flatMap(_ => get[Int])
        case Kind.Ch => among(Seq(n, n + 1))(using c, in.asInstanceOf[Choose :? c.Effs])
        case Kind.Thr => raise[Int, Int](n)(using c, in.asInstanceOf[Throws % Int :? c.Effs])
        case Kind.Rd => ask[Int](using c, in.asInstanceOf[Reader % Int :? c.Effs]).map(_ + n)
        case Kind.Rst =>
          // under a reset at Int: the context's answer is Int, which its type does not say here
          shift[Int, Int] { k0 =>
            val k = k0.asInstanceOf[Int => c.M[Int, Int, Int]]
            k(n).flatMap(x => k(x + 1)).map(_ * 5)
          }.asInstanceOf[c.Prog[Int]]
    case P.Handle(k, n, body) =>
      val inner = k :: scope
      k match
        case Kind.Write => writer[Int, Int](build(body, inner)).map((log, v) => v * 31 + log.sum * 7 + log.size)
        case Kind.St => state[Int](n)(build(body, inner)).map((s, v) => s * 31 + v)
        case Kind.Ch => choose[Int](build(body, inner)).map(_.foldLeft(17)((h, x) => h * 31 + x))
        case Kind.Thr => throws[Int, Int](build(body, inner)).map(_.fold(e => -e, identity))
        case Kind.Rd => reader[Int](n)(build(body, inner))
        case Kind.Rst => reset[Int](build(body, inner)).map(_ + n)

  /** a program's run on each monad: its value, or the failure's class — what they must agree on */
  def outcomes(p: P): List[Any] =
    def on(run: => Int): Any = try run catch case e: RuntimeException => e.getClass
    List(
      on(Backend.cont.run[Int](build(p, Nil))),
      on(ZeroBackend.run[Int](build(p, Nil))),
      on(RefBackend.run[Int](build(p, Nil))),
      on(BackendSuite.onLargeStack(Backend.cps0.run[Int](build(p, Nil)))))

  def disagree(p: P): Boolean = outcomes(p).distinct.size > 1

  /** the programs one step smaller: a handler its body, a part a value, a sequence one of its halves, a number 0 */
  def smaller(p: P): List[P] = p match
    case P.Pure(n) => if n != 0 then List(P.Pure(0)) else Nil
    case P.Op(i, n) => P.Pure(n) :: (if n != 0 then List(P.Op(i, 0)) else Nil)
    case P.Seq(a, b) => a :: b :: smaller(a).map(P.Seq(_, b)) ::: smaller(b).map(P.Seq(a, _))
    case P.Handle(k, n, body) =>
      unwrap(body, 0) :: P.Pure(0) :: (if n != 0 then List(P.Handle(k, 0, body)) else Nil) ::: smaller(body).map(P.Handle(k, n, _))

  /** the body of a handler taken out of it, `d` handlers deep in the body: an operation of the handler a value, one of
   * a handler further out one index nearer */
  def unwrap(p: P, d: Int): P = p match
    case P.Pure(_) => p
    case P.Op(i, n) => if i == d then P.Pure(n) else if i > d then P.Op(i - 1, n) else p
    case P.Seq(a, b) => P.Seq(unwrap(a, d), unwrap(b, d))
    case P.Handle(k, n, body) => P.Handle(k, n, unwrap(body, d + 1))

  /** a disagreeing program cut greedily to one no step smaller of which disagrees */
  @annotation.tailrec final def shrink(p: P): P = smaller(p).find(disagree) match
    case Some(q) => shrink(q)
    case None => p

  def count(p: P): (Int, Int) = p match
    case P.Pure(_) => (0, 0)
    case P.Op(i, _) => (0, if i > 0 then 1 else 0)
    case P.Seq(a, b) => val (h1, c1) = count(a); val (h2, c2) = count(b); (h1 + h2, c1 + c2)
    case P.Handle(_, _, body) => val (h, c) = count(body); (h + 1, c)

  test("1 000 programs made at random: Cont, Cont0, Ref and Cps0 agree"):
    val r = Random(20261009)
    var handlers = 0
    var crossing = 0
    for i <- 0 until 1000 do
      val p = make(r, Nil, 32)
      val (h, c) = count(p)
      handlers += h
      crossing += c
      if disagree(p) then
        val small = shrink(p)
        fail(s"the monads disagree: ${outcomes(small)} on\n$small\nshrunk from\n$p")
    // the programs are not trivial: handlers nested, operations crossing them
    assert(handlers > 3000 && crossing > 500, s"$handlers handlers, $crossing operations crossing one")
