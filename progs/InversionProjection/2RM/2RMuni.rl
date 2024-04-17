(Prog X) -> (Prog Y) with (i Prog' ProgRev Sign)

init: entry
      Y <- '0
      Prog' ^= Prog
      if Prog' goto loop else stop

loop: fi !ProgRev && Y = '0 from init else loopEnd
       ((Sign . i) . Prog') <- Prog'
       if Sign = 'Inv goto revOp else regOp

loopJoin: fi Sign = 'Inv from revOpEnd else regOpEnd
           ProgRev <- ((Sign . i) . ProgRev)
           if Prog' goto loopEnd else loopReload

loopReload: fi Prog' from loopReload else loopJoin
             ((Sign . i) . ProgRev) <- ProgRev
             Prog' <- ((Sign . i) . Prog')
             if ProgRev goto loopReload else loopEnd

loopEnd: fi ProgRev from loopJoin else loopReload
         if !ProgRev && X = '0 goto stop else loop

stop: fi Prog' from loopEnd else init
      Prog' ^= Prog
      '0 <- X
      exit

regOp: from loop //call Op
       if i = 'Swap goto regSwap else regOp1

regSwap: from regOp
         (X . Y) <- (Y . X)
         goto regOpEnd

regOp1: from regOp
        if i = 'Sum goto regComb else regAdd

regComb: from regOp1
         X += Y
         goto regOp2

regAdd: from regOp1
        X += i
        goto regOp2

regOp2: fi i = 'Sum from regComb else regAdd
        goto regOpEnd

regOpEnd: fi i = 'Swap from regSwap else regOp2
          goto loopJoin

//uncall Op
revOp: from loop
       if i = 'Swap goto revSwap else revOp1

revSwap: from revOp // Mergeable?
         (Y . X) <- (X . Y)
         goto revOpEnd

revOp1: from revOp
        if i = 'Sum goto revComb else revAdd

revComb: from revOp1
         X -= Y
         goto revOp2

revAdd: from revOp1
        X -= i
        goto revOp2

revOp2: fi i = 'Sum from revComb else revAdd
        goto revOpEnd

revOpEnd: fi i = 'Swap from revSwap else revOp2
          goto loopJoin
