# effects-once - an effect written once, its Program derived

One function, a handler in context to a handler of `Program`s (taking its effect wherever it is, `Removed`/`View`):
`Programs.reader`, `state`, `writer`, `throws` one line each; one's own effect written in context only.

Done when: `Programs` is the derivation and the operations; tests green.
