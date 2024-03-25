(x y z) -> (x y z)
// Swap x and y, loop on z

init: entry
      goto loop

loop: fi z from init else loop
      x += '1
      y += '1
      (x . y) <- (y . x)
      if z goto stop else loop

stop: from loop
      exit
