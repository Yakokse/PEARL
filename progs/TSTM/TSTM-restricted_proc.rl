proc TSTM
init: entry (Ini . (Fin. (Rules . (S_right. Involution))))
      S <- 'BLANK
      Q ^= Ini
      goto loop
stop: from loop
      'BLANK <- S
      Q ^= Fin
      exit (Ini . (Fin. (Rules . (S_right. Involution))))

loop: fi Q = Ini from init else act
      if Q = Fin goto stop else act

act: from loop
      PartialRule ^= (Q.(S.('nil.'nil)))
      (Rule.(PartialRule.(Rules.Involution))) <- call getRule (PartialRule.(Rules.Involution))
      PartialRule ^= (Q.(S.('nil.'nil)))
      State <- (Q.(S_left . (S . S_right)))
      (Rule.State) <- call step (Rule . State)
      (Q.(S_left . (S . S_right))) <- State

      PartialRule ^= ('nil.('nil.(S.Q)))
      (PartialRule.(Rules.Involution)) <- uncall getRule (Rule.(PartialRule.(Rules.Involution)))
      PartialRule ^= ('nil.('nil.(S.Q)))
      goto loop

proc getRule
// Input:
// partially specified 'Rule' to match
// Set of rules 'Rules'
// Involution
// Output
// (Rule. (PartialRule.(Rules.Involution)))
init: entry (PartialRule.(Rules.Involution))
      (Rule.(PartialRule.Rules)) <- call pickRule (PartialRule.Rules)
      InRules ^= 'True
      if Rule goto done else involute
involute: from init
          (InvolutedRules.Involution) <- call InvoluteRules (Rules.Involution)
          (Rule.(PartialRule.InvolutedRules)) <- call pickRule (PartialRule.InvolutedRules)
          InRules ^= 'True
          InRules ^= 'False
          (Rules.Involution) <- call InvoluteRules (InvolutedRules.Involution)
          goto done
done: fi InRules = 'True from init else involute
      (Rule.Rules) <- uncall In (Rule.(Rules.InRules))
      exit (Rule . (PartialRule .(Rules.Involution)))

// Input:
// partially specified 'Rule'
// Set of rules 'Rules'
// Output
// (matching rule. (From . (S .Rules)))

proc pickRule
init: entry ((From.(S.(S'.To))).Rules)
      goto matchRule

matchRule: fi !RulesRev from init else putInRev
      if !Rules goto done else pickTransition

pickTransition: from matchRule
    ((Q1 . (S1 . (S2 . Q2))) . Rules) <- Rules
    if (From = Q1 && (S1 = S || S1 = 'SLASH)) || (To = Q2 && (S2 = S' || S2 = 'RIGHT || S2 = 'LEFT)) goto setMatch else putInRev

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
      exit (Rule.((From.(S.(S'.To))).Rules))

proc In
// Input:
// L list
// A item
// Output:
// (A.(L.IsIn))
init: entry (A.L)
      (A.(L.Count)) <- call Count (A.L)
      if ('0 < Count) goto isIn else isNotIn
isIn: from init
      IsIn ^='True
      goto done
isNotIn: from init
         IsIn ^= 'False
         goto done
done: fi IsIn = 'True from isIn else isNotIn
      (A.L) <- uncall Count (A.(L.Count))
      exit (A.(L.IsIn))

proc Count
// Input:
// L list
// A item
// Output:
// (A.(L.Count))
init: entry (A.L)
      Count ^= '0
      goto loop
loop: fi L' from loop2 else init
      if L goto loop1 else done
loop1: from loop
        (head.L) <- L
        if head = A goto match else loop2
loop2: fi head = A from match else loop1
       L' <- (head.L')
       goto loop
match: from loop1
       Count += '1
       goto loop2
done: from loop
      L <- call Reverse L'
      exit (A.(L.Count))

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

proc InvoluteRules
init: entry(Rules.Involution)
    goto loop

loop: fi !RulesInv from init else invertRule
    if !Rules goto done else invertRule

invertRule: from loop
    (Rule.Rules) <- Rules
    (Rule'.Involution) <- call invertRule (Rule.Involution)
    RulesInv <- (Rule'.RulesInv)
    goto loop
done: from loop
    exit(RulesInv.Involution)
    //RulesInv contains inverted rules
    //Rules is empty
    //Involution is same as start of call


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

// ---------------------------------------------------------------------------------

proc step
// Input:
// Rule to step with
// current State
// Output:
// Rule stepped with
// new State
act: entry (Rule . State)
      (Q.(S_left . (S . S_right))) <- State
      (Q1'. (S1'. (S2'.Q2'))) <- Rule
      if S1' = 'SLASH goto move else write

write: from act
      Q ^= Q1'
      Q ^= Q2'
      S ^= S1'
      S ^= S2'
      goto loopEnd

move: from act
      Q ^= Q1'
      Q ^= Q2'
      if S2' = 'LEFT goto left else right

loopEnd: fi S1' = 'SLASH from move1 else write
    State <- (Q.(S_left . (S . S_right)))
    Rule <- (Q1'. (S1'. (S2'.Q2')))
    exit (Rule.State)


left: from move
      if S_right = 'nil && S = 'BLANK goto left_1b else left_1p

left_1b: from left // MERGE? 1
         S ^= 'BLANK
         goto left1

left_1p: from left
         S_right <- (S . S_right)
         goto left1

left1: fi S_right = 'nil from left_1b else left_1p
       if S_left = 'nil goto left_2b else left_2p

left_2b: from left1 // MERGE? 2
         S ^= 'BLANK
         goto left2

left_2p: from left1
         (S . S_left) <- S_left
         goto left2

left2: fi S_left = 'nil && S = 'BLANK from left_2b else left_2p
       goto move1

right: from move
       if S_left = 'nil && S = 'BLANK goto right_1b else right_1p

right_1b: from right // MERGE? 1
          S ^= 'BLANK
          goto right1

right_1p: from right
          S_left <- (S . S_left)
          goto right1

right1: fi S_left = 'nil from right_1b else right_1p
        if S_right = 'nil goto right_2b else right_2p

right_2b: from right1 // MERGE? 2
          S ^= 'BLANK
          goto right2

right_2p: from right1
          (S . S_right) <- S_right
          goto right2

right2: fi S_right = 'nil && S = 'BLANK from right_2b else right_2p
        goto move1

move1: fi S2' = 'LEFT from left2 else right2
       goto loopEnd