proc PickRule
init: entry (From.(S.Rules))
      goto matchRule

matchRule: fi !RulesRev from init else putInRev
      if !Rules goto done else pickTransition

pickTransition: from matchRule
    ((Q1 . (S1 . (S2 . Q2))) . Rules) <- Rules
    if From = Q1 && (S1 = S || S1 = 'SLASH) goto setMatch else putInRev

putInRev: fi Q1 = Q1' && Q2 = Q2' && S1 = S1' && S2 = S2' from setMatch else pickTransition
      RulesRev <- ((Q1 . (S1 . (S2 . Q2))) . RulesRev)
      goto matchRule

setMatch: from pickTransition
      Rule <- (Q1 . (S1 . (S2 . Q2)))
      Rule' ^= Rule
      (Q1' . (S1' . (S2' . Q2')))<- Rule'
      (Q1 . (S1 . (S2 . Q2))) <- Rule
      goto putInRev

done: from matchRule
      if Q1' && Q2' && S1' && S2' goto foundRule else fin
foundRule: from done
      Rule <- (Q1' . (S1' . (S2' . Q2')))
      goto fin

fin: fi Rule from foundRule else done
      Rules <- call Reverse RulesRev
      exit (Rule.(From.(S.Rules)))

proc Reverse
    init: entry L
          goto loop
    loop: fi L' from loop1 else init
        if L goto loop1 else done
    loop1: from loop
        (head.L) <- L
        L' <- (head.L')
        goto loop
    done: from loop
          exit L'