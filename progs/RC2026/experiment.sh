#!/bin/sh

PEARL invert int.rl invint.rl
PEARL spec int.rl fullsubtractor.rl fullsubtrator.spec >/dev/null
PEARL spec invint.rl invfulladder.rl fulladder.spec >/dev/null
PEARL normalize fullsubtractor.rl v1.tmp >/dev/null
PEARL normalize invfulladder.rl v2.tmp >/dev/null
diff v1.tmp v2.tmp && echo "Textually equivalent!" || echo "Failure!"
