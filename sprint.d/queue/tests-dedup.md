# tests-dedup

The kernel's suites (`okay.one`, 118 tests) and the facade's (`okay.core`, `words`) overlap: `TestAtmUses` and
`TestAtm` (the same protocol and pipeline in the kernel's words and okay's), `TestControl` in both packages,
`TestStd` beside the std's own. Decide what the kernel's suite checks (nodes, the machine, the reference) and
what the words' do (the fitting, the sugar), keep each case once where it belongs; the compile-fail checks stay
where their message is made. A day, no code under test touched.
