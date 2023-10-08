(ys n) -> (xs ys) with (x)
// xs is a list of n+1 x's

init: entry
      if n = '0 goto stop else loop

loop: fi xs = 'nil from init else loop
      (x . ys) <- ys
      xs <- (x . xs)
      n -= '1
      if n = '0 goto stop else loop

stop: fi xs = 'nil from init else loop
      n ^= '0
      exit