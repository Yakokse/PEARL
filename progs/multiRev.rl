(l1 l2) -> (l1 l2) with (tmpE tmpL)

init: entry
      if l1 goto rev1 else join

rev1: fi tmpL from rev1 else init
      (tmpE . l1) <- l1
      tmpL <- (tmpE . tmpL)
      if l1 goto rev1 else join

join: fi tmpL from rev1 else init
      l1 <- tmpL
      if l2 goto rev2 else stop

rev2: fi tmpL from rev2 else join
      (tmpE . l2) <- l2
      tmpL <- (tmpE . tmpL)
      if l2 goto rev2 else stop

stop: fi tmpL from rev2 else join
      l2 <- tmpL
      exit
