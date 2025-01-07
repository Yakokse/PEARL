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