(x n) -> (xs)
// xs is a list of n+1 x's

init: entry
      if n = '0 goto stop else loop

loop: fi xs = 'nil from init else loop
      xs <- (x . xs)
      x ^= hd xs
      n -= '1
      if n = '0 goto stop else loop

stop: fi xs = 'nil from init else loop
      xs <- (x . xs)
      n ^= '0
      exit