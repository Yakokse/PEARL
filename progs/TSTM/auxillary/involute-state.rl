proc InvoluteRules
init: entry(Rules.Involution)
    goto loop

loop: fi !RulesOut from init else resetsDone
    if !Rules goto done else pickRule

pickRule: from loop
    ((Q1 . (S1 . (S2 . Q2))).Rules) <- Rules
    goto loop1

invertAction: from foundInvolution2
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
    goto resetQ1AndQ2

resetsDone: from Q2reset
    RulesOut <- ( (Q1' . (S1' . (S2' . Q2'))). RulesOut) // insert involuted rule
    goto loop

done: from loop
    (RulesOut . Rules) <- (Rules . RulesOut)
    exit(Rules.Involution)
    //RulesOut contains inverted rules
    //Rules is empty
    //Involution is same as start of call


//--------------------------------------------------------------------------------
//                         PICKS OUT Q1' and Q2' does nothing else
//---------------------------------------------------------------------------------
loop1: fi !InvolutionRev from pickRule else putInRev
    if !Involution goto foundInvolution1 else findInvolution1

findInvolution1: from loop1
    ((From . To) . Involution) <- Involution
    if From = Q1 goto setMatch else putInRev

putInRev: fi Q2' = To from setMatch else findInvolution1
    InvolutionRev <- ((From . To) . InvolutionRev)
    goto loop1
setMatch: from findInvolution1
    Q2' ^= To
    goto putInRev

foundInvolution1: from loop1
    (Involution . InvolutionRev ) <- (InvolutionRev . Involution)
    goto loop2

loop2: fi !InvolutionRev from foundInvolution1 else putInRev2
    if !Involution goto foundInvolution2 else findInvolution2

findInvolution2: from loop2
    ((From . To) . Involution) <- Involution
    if From = Q2 goto setMatch2 else putInRev2

putInRev2: fi Q1' = To from setMatch2 else findInvolution2
    InvolutionRev <- ((From . To) . InvolutionRev)
    goto loop2
setMatch2: from findInvolution2
    Q1' ^= To
    goto putInRev2

foundInvolution2: from loop2
    (Involution . InvolutionRev ) <- (InvolutionRev . Involution)
    //Q1' contains mapping for Q2
    //Q2' contains mapping for Q1
    goto invertAction

// ---------------------------------------------------------------------------------
//--------------------------------------------------------------------------------
//                         RESETS OUT Q1 and Q2 does nothing else
//---------------------------------------------------------------------------------
resetQ1AndQ2:from invertionDone
    goto rloop1

rloop1: fi !InvolutionRev from resetQ1AndQ2 else rputInRev
    if !Involution goto Q1reset else findQ1

findQ1: from rloop1
    ((From . To) . Involution) <- Involution
    if To = Q2' goto unsetMatch else rputInRev

rputInRev: fi To = Q2' from unsetMatch else findQ1
    InvolutionRev <- ((From . To) . InvolutionRev)
    goto rloop1
unsetMatch: from findQ1
    Q1 ^= From //Resets Q1
    goto rputInRev

Q1reset: from rloop1
    (Involution . InvolutionRev ) <- (InvolutionRev . Involution)
    goto rloop2

rloop2: fi !InvolutionRev from Q1reset else rputInRev2
    if !Involution goto Q2reset else findQ2

findQ2: from rloop2
    ((From . To) . Involution) <- Involution
    if To = Q1' goto unsetMatch2 else rputInRev2

rputInRev2: fi Q1' = To from unsetMatch2 else findQ2
    InvolutionRev <- ((From . To) . InvolutionRev)
    goto rloop2
unsetMatch2: from findQ2
    Q2 ^= From //Resets Q2
    goto rputInRev2

Q2reset: from rloop2
    (Involution . InvolutionRev ) <- (InvolutionRev . Involution)
    //Q1' contains mapping for Q2
    //Q2' contains mapping for Q1
    goto resetsDone

// ---------------------------------------------------------------------------------
