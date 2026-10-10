package okay.interop

/** docs/interop.md's own text: its blocks compiled into `InteropDoc` (project/GuideDoc.scala), each checked line's
 * value the one the page says */
class TestInteropDoc extends munit.FunSuite:
  test("docs/interop.md: its checked lines"):
    assert(InteropDoc.checks.nonEmpty)
  for (code, run, expected) <- InteropDoc.checks do
    test(s"docs/interop.md: $code"):
      assertEquals(run(), expected)
