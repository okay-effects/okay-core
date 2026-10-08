- [ ] tagless-dispatch-tailcall — since final-tagless-effects-control,
      tailcallHandled (an operation answered in place, ~5.5 ns) costs ~1.2 ns
      more an operation: 54.5–54.8 µs at 75a3ddf, 66.3–67.6 µs after, back to
      back, allocation equal; tailcallHandledGeneral, tailcallForwarded,
      stateGetPut and the rest equal. -XX:+PrintInlining shows each operation
      through bridge methods (`At::flatMap (10) → At::flatMap (6) →
      Cont::flatMap`, `At::perform (7) → (9)`): `At` returns `Cont` where the
      class returns the abstract `M`, so erasure makes bridges, inline depth.
      Shrinking Cont.flatMap below 35 bytes did not help. To try: a variant of
      the benchmark on the concrete `Cont` to confirm the split; instance
      methods typed so no bridge is needed; JIT flags only to diagnose.
