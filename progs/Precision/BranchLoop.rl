(x y z) -> (x y z)

init: entry
      goto loop1

loop1: fi z from init else loop2
       if z goto swap else add

swap: from loop1
      (x . y) <- (y . x)
      goto loop2

add: from loop1
     x += y
     goto loop2

loop2: fi z from swap else add
       if z goto stop else loop1

stop: from loop2
      exit
