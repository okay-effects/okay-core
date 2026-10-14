package okay.docs

/** docs/guide.md's own text: its blocks compiled into `GuideDoc` (project/GuideDoc.scala), each checked line's value
 * the one the guide says */
class TestGuideDoc extends munit.FunSuite:
  test("the guide has its checks"):
    assert(GuideDoc.checks.size >= 10, GuideDoc.checks.size)

  for (code, run, expected) <- GuideDoc.checks do
    test(s"guide: $code"):
      assertEquals(run(), expected)
