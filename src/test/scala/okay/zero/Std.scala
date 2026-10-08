package okay.zero

/** the effects the tests use, and their handlers — written as a library outside the kernel would be */
object Std:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  enum Choose[+A]:
    case Among[A](as: List[A]) extends Choose[A]
  enum State[+A]:
    case Get extends State[Int]
    case Put(n: Int) extends State[Unit]

  def number[R <: Row, J, T](using Ctx[R, J], Ask :? R): Eff[R, J, T, T, Int] = perform(Ask.Number)
  def say[R <: Row, J, T](s: String)(using Ctx[R, J], Say :? R): Eff[R, J, T, T, Unit] = perform(Say.Line(s))
  def among[R <: Row, J, T, A](as: List[A])(using Ctx[R, J], Choose :? R): Eff[R, J, T, T, A] = perform(Choose.Among(as))
  def get[R <: Row, J, T](using Ctx[R, J], State :? R): Eff[R, J, T, T, Int] = perform(State.Get)
  def put[R <: Row, J, T](n: Int)(using Ctx[R, J], State :? R): Eff[R, J, T, T, Unit] = perform(State.Put(n))

  def asking[R <: Row, J, T, A](using Ctx[R, J])(n: Int)(body: Body[Ask, R, J, T, A, A]): Eff[R, J, T, T, A] =
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], Ask, A, A]:
      def ret(a: A): A = a
      def apply[X](op: Ask[X], k: X => Eff[R, J, T, T, A]): Eff[R, J, T, T, A] = op match
        case Ask.Number => k(n))(body)
  def saying[R <: Row, J, T, A](using Ctx[R, J])(body: Body[Say, R, J, T, A, (List[String], A)]): Eff[R, J, T, T, (List[String], A)] =
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], Say, A, (List[String], A)]:
      def ret(a: A): (List[String], A) = (Nil, a)
      def apply[X](op: Say[X], k: X => Eff[R, J, T, T, (List[String], A)]): Eff[R, J, T, T, (List[String], A)] = op match
        case Say.Line(s) => k(()).map((log, a) => (s :: log, a)))(body)
  def choosing[R <: Row, J, T, A](using Ctx[R, J])(body: Body[Choose, R, J, T, A, List[A]]): Eff[R, J, T, T, List[A]] =
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], Choose, A, List[A]]:
      def ret(a: A): List[A] = List(a)
      def apply[X](op: Choose[X], k: X => Eff[R, J, T, T, List[A]]): Eff[R, J, T, T, List[A]] = op match
        case Choose.Among(as) => as.foldLeft(pure[R, J, T, List[A]](Nil))((acc, x) => acc.flatMap(l => k(x).map(l ++ _))))(body)
  /** functional state: the answer a function of the state, the state threaded through `k` — no cell */
  def stating[R <: Row, J, T, A](using Ctx[R, J])(s0: Int)(body: Body[State, R, J, T, A, Int => Eff[R, J, T, T, (Int, A)]]): Eff[R, J, T, T, (Int, A)] =
    type F = Int => Eff[R, J, T, T, (Int, A)]
    handle(new Handler[[X] =>> Eff[R, J, T, T, X], State, A, F]:
      def ret(a: A): F = s => pure((s, a))
      def apply[X](op: State[X], k: X => Eff[R, J, T, T, F]): Eff[R, J, T, T, F] = op match
        case State.Get => pure(s => k(s).flatMap(f => f(s)))
        case State.Put(n) => pure(_ => k(()).flatMap(f => f(n))))(body).flatMap(f => f(s0))
