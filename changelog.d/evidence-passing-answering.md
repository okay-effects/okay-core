## evidence-passing-answering - dropped: handlers here are dynamic

- The idea: the context `handle` makes holds the handler, and `perform`
  answers an operation of a tail-resumptive handler through it, crossing no
  fold between (Koka's, Effekt's evidence). Dropped before any code, by the
  library's semantics, which TestEffects pins: "handlers are dynamic: a
  generator's rest asks the reader it is run under, not the one it was built
  under". A generator's rest escapes its `reader(7)` as data in the answer —
  rows do not forbid it — and is run under `reader(8)`; it must get 8.
  Evidence taken from the context a program is BUILT in would answer 7, and a
  `State` would write to the cell of a run already over.
- Koka keeps handlers dynamic by passing the vector at RUN time, a hidden
  parameter of the computation; here that would be the step/fold loop
  carrying it — another architecture. Effekt's handlers are lexical, with
  types that keep a capability from escaping; rows here do not.
