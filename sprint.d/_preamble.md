# Sprint

One file per item: `queue/<slug>.md` to pick next, in the order of `_order`;
`doing/<slug>.md` what is being worked on. Picking is
`git mv queue/<slug>.md doing/<slug>.md`; landing is `git rm` of it and
`changelog.d/<slug>.md` with what was done and what was measured.

Every item: tests green, a full `sbt "bench -prof gc"` against the last run in
bench.d, a commit to master. Pushed when asked.
