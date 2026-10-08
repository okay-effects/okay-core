- [ ] shift-under-handler-measured — a test and a benchmark for `shift`
      under a handler: since Reset lost its `ret` (95df988), each resumption
      of such a capture builds a `Map` (`captured` in Effects.scala), and no
      benchmark covers it. Measure it before the type-class work moves it.
