## guide-effects-from-effects - the guide: effects built on effects, four ways, each checked

- docs/guide.md, *Effects from effects*: operations of existing effects as a function (`incr`); an effect of one's
  own whose handler performs another outside it (`Audit` to `Writer`); one whose handler installs another inside,
  hidden by a private type, its body an `A ! E` (`Ticket` on `State % Issued`; the caller's own `State % Int`
  untouched); an existing effect changed for a part of the program, an interceptor (`shouting`).
- project/GuideDoc.scala takes a declaration's `private`/`final` too.
