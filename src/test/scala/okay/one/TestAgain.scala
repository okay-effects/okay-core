package okay.one

import okay.one.Cont.*
import okay.one.handler.Deep
import okay.one.row.*
import okay.one.std.*

/** `Removed.Again`: a handler of an effect at any place in the row, the row kept — its clauses' own operations of
 * the effect to the handler outside */
class TestAgain extends munit.FunSuite:
  type R = Writer % String +: Reader % Int +: Pure
  val reader: In[Reader % Int, R] = In.There(In.Here())
  val writer: In[Writer % String, R] = In.Here()

  def body: Int ! R =
    for
      a <- perform[Reader % Int, R, Int](Reader.Ask())(using reader)
      _ <- perform[Writer % String, R, Unit](Writer.Tell(s"a=$a"))(using writer)
      b <- perform[Reader % Int, R, Int](Reader.Ask())(using reader)
    yield a + b

  /** the reader asked again under an interceptor doubling the value it asks outside */
  def doubling[A] = new Deep[Reader % Int, R, A, A]:
    def ret(a: A) = pure(a)
    def apply[X](op: Reader[Int, X], k: X => A ! R) = op match
      case Reader.Ask() => perform[Reader % Int, R, Int](Reader.Ask())(using reader).flatMap(v => k(2 * v))

  test("an effect past the head handled again: the body's operations to the interceptor, its own outside") {
    val p: Int ! R = body.handle(doubling[Int])(using Removed.again(reader))
    assertEquals(p.handle(Writer.collect[String, Reader % Int +: Pure, Int]).handle(Reader(5)).run, (List("a=10"), 20))
  }

  test("the same at the head: the writer intercepted, its tells prefixed and told outside") {
    def prefixing[A] = new Deep[Writer % String, R, A, A]:
      def ret(a: A) = pure(a)
      def apply[X](op: Writer[String, X], k: X => A ! R) = op match
        case Writer.Tell(w) => perform[Writer % String, R, Unit](Writer.Tell("> " + w))(using writer).flatMap(k)
    val p: Int ! R = body.handle(prefixing[Int])(using Removed.again(writer))
    assertEquals(p.handle(Writer.collect[String, Reader % Int +: Pure, Int]).handle(Reader(5)).run, (List("> a=5"), 10))
  }

  test("relocation: the same place is the effect's, another place is not") {
    assert(In.same(reader, reader) != null)
    assert(In.same(writer, writer) != null)
    assertEquals(Removed.relocate(writer, Removed.again(reader)), In.There(writer))
    assertEquals(Removed.relocate(reader, Removed.again(reader)), In.Here())
  }
