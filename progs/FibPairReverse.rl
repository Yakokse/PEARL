proc main
      init: entry (a.b)
            n <- uncall FibPair (a.b)
            exit n

proc FibPair
      init: entry n
            a ^= '0
            b ^= '1
            if n = '0 goto stop else loop

      loop: fi (a . b) = '(0 . 1) from init else loop
            n -= '1
            (a . b) <- call nextPair (a.b)
            if n = '0 goto stop else loop

      stop: fi (a . b) = '(0 . 1) from init else loop
            n ^= '0
            exit (a.b)
proc nextPair
      init: entry (a.b)
            a += b
            exit (b.a)