import sbt._

/** THE GUIDE AS CODE: docs/guide.md's `scala` blocks made one object, `okay.docs.GuideDoc`, generated into the test
 * sources — so the guide's text is what compiles, not a copy of it. Its imports go first; its declarations (`def`,
 * `enum`, `type`, their indented lines) are the object's members; each line of its own that is an expression with
 * a comment, `expr   // value`, is a check, `expr` against `value` — run by TestGuideDoc. A value `the same` is
 * the one before it; a value's trailing `, on Cont` (the monad named) is not part of it */
object GuideDoc {
  private val Declaration = "^(private |final )*(def |enum |type |case class |given |object |trait |class ).*".r
  private val Checked = "^(.*?\\S)\\s+//\\s*(.*)$".r
  private val OnMonad = "^(.*), on \\w+$".r

  def generate(guide: File, out: File, pkg: String = "okay.docs", name: String = "GuideDoc"): Seq[File] = {
    val blocks = "(?s)```scala\n(.*?)```".r.findAllMatchIn(IO.read(guide)).map(_.group(1)).toList
    val imports = List.newBuilder[String]
    val members = List.newBuilder[String]
    val checks = List.newBuilder[String]
    var expected = ""
    for (block <- blocks) {
      var inDeclaration = false
      for (line <- block.linesIterator) {
        val top = line.nonEmpty && !line.head.isWhitespace
        if (line.trim.isEmpty) { if (inDeclaration) members += "" }
        else if (top && line.startsWith("import ")) { imports += line; inDeclaration = false }
        else if (top && Declaration.pattern.matcher(line).matches) { members += "  " + line; inDeclaration = true }
        else if (!top && inDeclaration) members += "  " + line
        else if (top && line.startsWith("//")) ()
        else line match {
          case Checked(expr, value) =>
            inDeclaration = false
            expected = value.trim match {
              case v if v.startsWith("the same") => expected
              case OnMonad(v) => v
              case v => v
            }
            val quoted = expr.replace("\\", "\\\\").replace("\"", "\\\"")
            checks += s"""    ("$quoted", () => $expr, $expected)"""
          case _ =>
            sys.error(s"docs/guide.md: a line neither a declaration nor a checked expression: $line")
        }
      }
    }
    val code =
      s"""// GENERATED from ${guide.getName} by project/GuideDoc.scala — edit the guide, not this
         |package $pkg
         |
         |${imports.result().distinct.mkString("\n")}
         |
         |object $name:
         |${members.result().mkString("\n")}
         |
         |  /** the guide's checked lines: the code, its value, the value the guide says */
         |  val checks: List[(String, () => Any, Any)] = List(
         |${checks.result().mkString(",\n")}
         |  )
         |""".stripMargin
    IO.write(out, code)
    Seq(out)
  }
}
