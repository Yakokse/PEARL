(gates lines) -> (gates lines)
with (gate gatesRev ls l1 l2 l3 v1 v2 v3)
init: entry
      goto act1
act1: fi gatesRev from act2 else init
      ((gate . ls) . gates) <- gates
      if gate = 'CCNOT goto toff else cnot
toff: from act1
      [l1, l2, l3] <- ls
      v1 <- lines[l1]
      v2 <- lines[l2]
      v3 <- lines[l3]
      v3 ^= v1 && v2
      lines[l3] <- v3
      lines[l2] <- v2
      lines[l1] <- v1
      ls <- [l1, l2, l3]
      goto act2
cnot: from act1
      [l1, l2] <- ls
      v1 <- lines[l1]
      v2 <- lines[l2]
      v2 ^= v1
      lines[l2] <- v2
      lines[l1] <- v1
      ls <- [l1, l2]
      goto act2
act2: fi gate = 'CCNOT from toff else cnot
      gatesRev <- ((gate . ls) . gatesRev)
      if gates goto act1 else act3
act3: fi gates from act3 else act2
      (gate . gatesRev) <- gatesRev
      gates <- (gate . gates)
      if gatesRev goto act3 else stop
stop: from act3
      exit
