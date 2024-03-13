(x1 y1) -> (x2 y1 y2) with (e)

init: entry
      if x1 goto loop else stop

loop: fi x2 from loop else init
      (e . x1) <- x1
      x2 <- (e . x2)
      (e . y1) <- y1
      y2 <- (e . y2)
      if x1 goto loop else stop

stop: fi x2 from loop else init
      exit
