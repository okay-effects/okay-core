# control-one-face

Delimited control has two faces: `Shifts[S, T]` as an effect of the row (`reset`/`shift`/`shift0`, `Deep`), and
`Atm` with `dollar`/`shift0` (the kernel's `$`). The first is the second with `Fixed[S]` and a `reset` as the `$`;
`shift` (the body under the reset again, Danvy–Filinski) is `shift0` with the `$` put back in the body. One face
would drop `Shift.scala`, `Control.reset/shift`, the `Sh[S]` rows in tests and the guide; it would break code
written `reset[Int, T](…)`. A matter of taste and of the users there are, not of the machine: decide, then a
day. The guide's "two faces" section stays until then.
