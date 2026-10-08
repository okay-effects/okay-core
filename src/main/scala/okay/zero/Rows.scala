package okay.zero

/** a ROW: the effects of a context, nominal — `Ask +: Say +: Empty`. Control is in any row */
sealed trait Row
sealed trait +:[E[_], T <: Row] extends Row
sealed trait Empty extends Row

/** the effect `E` in the row `R`, a path to it: the compiler builds it, the first by priority */
enum In[E[_], R <: Row]:
  case Here[E[_], T <: Row]() extends In[E, E +: T]
  case There[E[_], E2[_], T <: Row](in: In[E, T]) extends In[E, E2 +: T]
infix type :?[E[_], R <: Row] = In[E, R]
object In extends InLow:
  given here[E[_], T <: Row]: In[E, E +: T] = Here()
sealed trait InLow:
  given there[E[_], E2[_], T <: Row](using in: In[E, T]): In[E, E2 +: T] = In.There(in)
