// Input:
// Current state 'From'
// Currently read symbol 'S'
// Set of rules 'Rules'
// Output
// (Reverse rules . (S . (From .Matching rule)))

proc pickRule
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

noRuleFound: from done
      goto fin


fin: fi Rule from foundRule else done
      exit (RulesRev.(From.(S.Rule)))