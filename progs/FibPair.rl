(n) -> (a b)

init: entry
      a ^= '0
      b ^= '1
      if n = '0 goto stop else loop

loop: fi (a . b) = '(0 . 1) from init else loop
      n -= '1
      a += b
      (a . b) <- (b . a)
      if n = '0 goto stop else loop

stop: fi (a . b) = '(0 . 1) from init else loop
      n ^= '0
      exit
