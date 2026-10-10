## cont-from-zero - Cont borrows Cont0's lookup cache and its value into the frame — across 16 handlers 17.4 → 5.7 µs, state −13%

- `inPlace` walked the handlers in force for every operation past the first. Now, as `okay.zero`'s machine: the
  path's last lookup kept on the path (`In.found`), from which handlers; the same path through the same handlers
  finds the same — a loop's operations a compare, whatever the number of handlers.
- An operation answered in place gave its value to a `Return` for the next turn of the loop; now straight to the
  `Then` on top, if one is.
- Measured, Cont, all of MonadsBenchmark, before / after (µs): forwarded16 17.38 / 5.71, forwarded4 7.00 / 5.50,
  state 14.80 / 12.95 (214 → 198 B), stateHoisted 15.07 / 12.97, stateBang 13.91 / 12.95, stateReaderBang
  20.37 / 17.96; the rest within noise. Cont0's: forwarded16 5.8, state 12.6.
