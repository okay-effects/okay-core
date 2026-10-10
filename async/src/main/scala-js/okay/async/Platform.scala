package okay.async

/** THE PLATFORM'S WAITING, JS's: one thread — while it waited nothing else would run, so nothing can be waited for: a
 * wait is the run not completed at once, a failure that says so */
object Platform:
  final val waits = false
  def await(lock: AnyRef): Unit =
    throw IllegalStateException("nothing to wait for on JS: the run did not complete at once — use runAsync or subscribe")
  def wake(lock: AnyRef): Unit = ()
