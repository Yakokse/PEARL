init: entry
      if n = 0 goto stop else loop

loop: fi empty s from init else loop
      push x s
      x += top s
      n -= 1
      if n = 0 goto stop else loop

stop: fi empty s from init else loop
      exit