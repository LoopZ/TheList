#!/bin/sh

set -e

# Let's build makelist ourselves as we want to be sure that it's the latest
(cd utilities/bin
  [ -f makelist ] && mv makelist makelist.repo
  lazbuild ../makelist/makelist.lpr
  mv ../makelist/makelist .
)

# Now let's build the TheList
./utilities/bin/makelist -s source -o TheList

echo done
