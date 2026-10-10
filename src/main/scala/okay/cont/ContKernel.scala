package okay.cont

import okay.core.{Pure, +:}
import okay.core.kernel.{Dyn, KHandler, Machine}
import Cont.*

/** CONT AS THE KERNEL'S MACHINE (okay.core.kernel's `OnCore.cont`): its nodes made untyped — the kernel's programs
 * carry no row or answer types, okay-core's are phantom at run time: THE TRUSTED STEP of this object — and run on
 * Cont's top */
private[okay] object ContMachine extends Machine:
  private type C = Cont[Pure, Any, Any, Any]
  private def c(p: AnyRef): C = p.asInstanceOf[C]
  def pure(a: Any): okay.kernel.Prog[Any] = Return[Pure, Any, Any](a)
  def bind(p: AnyRef, f: Any => AnyRef): okay.kernel.Prog[Any] = Bind[Pure, Any, Any, Any, Any, Any](c(p), f.asInstanceOf[Any => C])
  def map(p: AnyRef, f: Any => Any): okay.kernel.Prog[Any] = Map[Pure, Any, Any, Any, Any](c(p), f)
  def delay(p: () => AnyRef): okay.kernel.Prog[Any] = Delay[Pure, Any, Any, Any](p.asInstanceOf[() => C])
  def perform(op: okay.kernel.Op[?]): okay.kernel.Prog[Any] = KInject[Pure, Any, Any](op)
  def performThen(op: okay.kernel.Op[?], k: Any => AnyRef): okay.kernel.Prog[Any] = KSuspend[Pure, Any, Any, Any](op, false, k.asInstanceOf[Any => C])
  def handle(h: KHandler): okay.kernel.Prog[Any] =
    val ch = h.asInstanceOf[okay.core.Handler[[Z] =>> Cont[Pure, Any, Any, Z], Dyn, Any, Any]]
    Fold[Dyn, Pure, Any, Any, Any, Any](h.asInstanceOf[() => Cont[Dyn +: Pure, Any, Any, Any]], h, ch,
      h.asInstanceOf[okay.core.Value[Dyn]])
  def run(p: AnyRef): Any = c(p).value
  def owns(p: Any): Boolean = p.isInstanceOf[Cont[?, ?, ?, ?]]
  def node(p: AnyRef): okay.kernel.Prog[Any] = c(p)
  def fuse(p: AnyRef, k: Any => AnyRef): okay.kernel.Prog[Any] | Null = p match
    case o: KInject[?, ?, ?] => KSuspend[Pure, Any, Any, Any](o.op, false, k.asInstanceOf[Any => C], true)
    case _ => null
