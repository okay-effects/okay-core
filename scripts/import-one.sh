#!/bin/sh
# OKAY-ONE INTO OKAY-CORE, as it is: the kernel's sources to the package `okay.one` beside `okay.cont` and `okay.zero`,
# its suites with them (docs/one.md §5, step 1) — its HEAD, not its working tree. Mechanical, re-run on each commit of okay-one until the machine
# settles; what is imported is written to docs/one-imported.txt.
#   scripts/import-one.sh [path-to-okay-one]
set -e
cd "$(dirname "$0")/.."
ONE=${1:-../okay-one}
rm -rf src/main/scala/okay/one src/test/scala/okay/one
mkdir -p src/main/scala/okay/one src/test/scala/okay/one
# HEAD's, not the working tree's: what okay-one has committed
git -C "$ONE" archive HEAD src/main/scala/okay/one src/test/scala/okay/one | tar -x -C . 
printf 'okay-one %s (%s)\n' "$(git -C "$ONE" rev-parse --short HEAD)" "$(git -C "$ONE" log -1 --format=%ci)" > docs/one-imported.txt
cat docs/one-imported.txt
