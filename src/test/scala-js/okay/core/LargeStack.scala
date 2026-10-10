package okay.core

/** JS has one thread and no stack to choose: `a` where it is — a suite that needs a large stack is the JVM's */
object LargeStack:
  def apply[A](a: => A): A = a
