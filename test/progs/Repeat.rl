// repeat x n = list of n+1 x's

init: entry
      if n = '0 goto stop else loop

loop: fi list = 'nil from init else loop
      list <- (x . list)
      x ^= hd list
      n -= '1
      if n = '0 goto stop else loop

stop: fi list = 'nil from init else loop
      list <- (x . list)
      exit