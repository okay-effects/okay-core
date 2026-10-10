package okay.core

/** `a` on a thread of a 1 GB stack — Native takes no more than a little under 2 — its value or its failure here: the JVM's
 * and Native's threads */
object LargeStack:
  def apply[A](a: => A): A =
    var result: Option[Either[Throwable, A]] = None
    val thread = Thread(null, () => result = Some(try Right(a) catch case e: Throwable => Left(e)), "large-stack", 1L << 30)
    thread.start()
    thread.join()
    result.get.fold(e => throw e, identity)
