## zero-bind-delimiter - a handler or a reset bound, entered at once; the reading simpler

- In the kernel's loop and the machine's, `Bind(Handle | Reset, f)`: the delimiter's loop first, its value given to
  `f` with no frame pushed; else the frame as before. throws 224 → 200 B a run, 14.6 → 14.4 ns (Cont 13.7).
- The reading of a delimiter's answers (`handleRead`, `resetRead`): its requests a polymorphic function, the rest
  read again by the reader itself — no trait, no `again` parameter. The same speed (the reading is `bail`'s, off the
  fast path).
- Tried and dropped: `deliver0` back as a loop of its own beside `deliver` — throws −2%, nothing else; not worth a
  second loop.
