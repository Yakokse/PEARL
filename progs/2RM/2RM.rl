(Prog X) -> (Prog Y) 
with (i ProgRev Sign)

init: entry
      Y ^= '0
      if Prog goto loop else stop

loop: fi !ProgRev && Y = '0 from init else loopEnd
      goto loop1

loop1: fi ProgRev from loop1End else loop
       ((i . Sign) . Prog) <- Prog
       if Sign = 'Neg goto revOp else regOp

loop1Join: fi Sign = 'Neg from revOpEnd else regOpEnd
           ProgRev <- ((i . Sign) . ProgRev)
           if Prog goto loop1End else loop1Reload

loop1Reload: fi Prog from loop1Reload else loop1Join
             ((i . Sign) . ProgRev) <- ProgRev
             Prog <- ((i . Sign) . Prog)
             if ProgRev goto loop1Reload else loop1End

loopEnd: fi ProgRev from loop1Join else loop1Reload
         if !ProgRev && X = '0 goto stop else loop

stop: fi Prog from loopEnd else init
      X ^= '0
      exit

regOp: from loop1 //call Op
       if i = '0 goto regSwap else regOp1

regSwap: from regOp
         (X . Y) <- (Y . X)
         goto regOpEnd

regOp1: from regOp
        if i = '1 goto regComb else regAdd

regComb: from regOp1
         X += Y
         goto regOp2

regAdd: from regOp1
        X += i - '1
        goto regOp2

regOp2: fi i = '1 from regComb else regAdd
        goto regOpEnd        

regOpEnd: fi i = '0 from regSwap else regOp2
          goto loop1Join

//uncall Op
revOp: from loop1
       if i = '0 goto revSwap else revOp1

revSwap: from revOp // Mergeable?
         (Y . X) <- (X . Y)
         goto revOpEnd

revOp1: fromRevOp
        if i = '1 goto revComb else revAdd

revComb: from revOp1
         X -= Y
         goto revOp2

revAdd: from revOp1
        X -= i - '1
        goto revOp2

revOp2: fi i = '1 from revComb else revAdd
        goto revOpEnd

revOpEnd: fi i = '0 from regSwap else revOp2
          goto loop1Join
