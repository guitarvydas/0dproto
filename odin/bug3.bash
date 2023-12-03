#!/bin/bash
# clone https://github.com/guitarvydas/0d/tree/bug3
# git checkout bug3
# cd odin # ensure that pwd is ..../0d/odin
lldb agency.bin ../src/agency.drawio
# > run
# > bt

#frame #5 calls fmt.assertf, then frame #0 crashes (len=... is suspicious, but looks OK in frame #1)
