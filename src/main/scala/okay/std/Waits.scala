package okay.std

/** whether this platform can wait for another thread: the JVM's and Native's can; JS has one thread, nothing comes
 * while it waits. One object a platform (the cross build's platform sources) */
object Platform:
  final val waits = true

/** A CELL SWAPPED AT ONCE: what a run and a callback race on — `compareAndSet` decides which of them came first.
 * `synchronized`, which every platform has (a no-op on JS's one thread) */
final class Swap[A](init: A):
  private var value: A = init
  def get: A = synchronized(value)
  def compareAndSet(expect: A, update: A): Boolean = synchronized:
    if value == expect then { value = update; true } else false

/** A MAILBOX between a run and the thread waiting on it: values `put`, `take`n in order — waiting for one where the
 * platform can wait; where it cannot (JS), `take` of an empty one fails: the run did not complete at once */
final class Mailbox[A]:
  private val held = scala.collection.mutable.Queue.empty[A]
  def put(a: A): Unit = synchronized:
    held.enqueue(a)
    notifyAll()
  def take(): A = synchronized:
    while held.isEmpty do
      if Platform.waits then wait()
      else throw IllegalStateException("nothing to wait for on this platform: the run did not complete at once")
    held.dequeue()
