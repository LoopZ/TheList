#!/bin/sh

set -e

# Let's build makelist ourselves as we want to be sure that it's the latest
(cd utilities
  [ -f bin/makelist ] && mv bin/makelist bin/makelist.repo
  fpc -obin/makelist makelist/makelist.lpr
)

echo

# Now let's build the TheList
./utilities/bin/makelist -s source -o TheList

