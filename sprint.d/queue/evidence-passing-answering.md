- [ ] evidence-passing-answering — an operation crosses every handler
      between it and its own, ~12 ns and 48 B each (forwardedDepth depth=16:
      88 ns, 840 B). For a tail-resumptive (`Answering`) handler, the `At`
      made by `handle` can hold the handler, and `perform` answer through it,
      lazily (`Delay(() => Return(h.value(op)))`), crossing nothing — Koka's
      and Effekt's evidence passing. Last, in a branch; check multi-shot
      resumptions, generators, a program run twice; decide by forwardedDepth.
