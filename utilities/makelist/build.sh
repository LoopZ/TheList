#!/bin/bash

#   Copyright (c) 2025-2026 Jerome Shidel
#   The Clear BSD License
#   All rights reserved.

rm *.o *.or *.ppu makelist 2>/dev/null
fpc makelist.lpr || exit 1
rm *.o *.or *.ppu 2>/dev/null
ls -al makelist

