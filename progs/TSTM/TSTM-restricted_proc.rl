proc TSTM
init: entry (Ini . (Fin. (Rules . (S_right. Involution))))
      S <- 'BLANK
      Q ^= Ini
      goto loop
stop: from loop
      'BLANK <- S
      Q ^= Fin
      exit (Ini . (Fin. (Rules . (S_right. Involution))))

loop: fi Q = Ini from init else cleanup
      if Q = Fin goto stop else pickRule
