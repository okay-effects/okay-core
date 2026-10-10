package okay.async

/** THE PLATFORM'S WAITING, the JVM's and Native's: a thread waits on a lock until another wakes it */
object Platform:
  final val waits = true
  /** wait on `lock`, held by the caller, until woken */
  def await(lock: AnyRef): Unit = lock.wait()
  /** wake those waiting on `lock`, held by the caller */
  def wake(lock: AnyRef): Unit = lock.notifyAll()
