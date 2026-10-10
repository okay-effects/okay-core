package okay.async

/** docs/async.md's own text: its blocks compiled into `AsyncDoc` (project/GuideDoc.scala), each checked line's value
 * the one the page says */
class TestAsyncDoc extends munit.FunSuite:
  test("docs/async.md: its checked lines"):
    assert(AsyncDoc.checks.nonEmpty)
  for (code, run, expected) <- AsyncDoc.checks do
    test(s"docs/async.md: $code"):
      assertEquals(run(), expected)
