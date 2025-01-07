proc invertRule
init: entry (Rule.Involution)
      (Q1.(S1.(S2.Q2))) <- Rule
      (Q1'.Involution) <- call involuteState (Q2.Involution)
      (Q2'.Involution) <- call involuteState (Q1.Involution)
      goto invertAction
done: from invertionDone
      Rule <- (Q1'.(S1'.(S2'.Q2')))
      exit (Rule.Involution)

invertAction: from init
    if S1 = 'SLASH goto invertMove else invertWrite

invertMove: from invertAction
    S1' <- S1 //maybe pattern match instead?
    if S2 = 'LEFT goto invertLeft else invertRight

invertRight: from invertMove
    'RIGHT <- S2 //Not working for some reason? S2 ^= 'RIGHT instead?
    S2' <- 'LEFT
    goto invertMove1

invertLeft: from invertMove
    S2' <- 'RIGHT
    'LEFT <- S2
    goto invertMove1

invertMove1: fi S2' = 'LEFT from invertRight else invertLeft
    goto invertionDone

invertWrite: from invertAction
    (S1' . S2') <- (S2 . S1)
    goto invertionDone

invertionDone: fi S1' = 'SLASH from invertMove1 else invertWrite
    goto done


proc involuteState
init: entry (Q.Involution)
      goto loop
loop: fi InvolutionRev from putInRev else init
      if Involution goto take else done

take: from loop
      ((From . To) . Involution) <- Involution
      if Q = From goto match else putInRev
match: from take
    Q ^= From
    Q' ^= To
    goto putInRev

putInRev: fi Q' = To from match else take
          InvolutionRev <- ((From.To).InvolutionRev)
          goto loop
done: from loop
      Involution <- InvolutionRev //Reverse ??
      exit (Q'.Involution)