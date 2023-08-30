init: entry
      x_2 += 1
      goto loop

loop: fi x_1 = 0 from init else loop
      n -= 1
      x_1 += x_2
      push x_1 stack
      push x_2 stack
      pop x_1 stack
      pop x_2 stack
      if n = 0 goto stop else loop

stop: from loop
      exit