(Prog X) -> (Prog Y) 
with (i ProgRev Sign)
// ISSUE: NEGATIVE NUMBERS
// SOLUTION: NEGATIVE NUMBERS / MODULUS OPERATOR

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

revOp: from loop1 //uncall Op
       goto revOpEnd

revOpEnd: from revOp
          goto loop1Join

regOp: from loop1 //call Op
       goto regOpEnd

regOpEnd: from regOp
          goto loop1Join