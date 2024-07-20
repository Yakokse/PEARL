(Start End Rules S_right)
    -> (Start End Rules S_right)
    with (Q Q1 Q2 S1 S2 S S_left
          RulesRev Rule R)

init: entry
      S ^= 'BLANK
      Q ^= Start
      goto act1

act1: fi !RulesRev && Q = Start
         from init else act3
      (Rule . Rules) <- Rules
      (Q1 . (R . Q2)) <- Rule
      if R = 'LEFT || R = 'RIGHT
         goto shft else symbol

act2: fi R = 'LEFT || R = 'RIGHT
         from shft3 else symbol2
      Rule <- (Q1 . (R . Q2))
      RulesRev <- (Rule . RulesRev)
      if Rules goto act3 else reload

reload: fi Rules
           from reload else act2
        (Rule . RulesRev) <- RulesRev
        Rules <- (Rule . Rules)
        if RulesRev
           goto reload else act3

act3: fi !RulesRev
         from reload else act2
      if !RulesRev && Q = End
         goto stop else act1

stop: from act3
      Q ^= End
      S ^= 'BLANK
      exit

symbol: from act1
        (S1 . S2) <- R
        if Q = Q1 && S = S1
           goto symbol1 else symbol2

symbol1: from symbol
         Q ^= Q1
         Q ^= Q2
         S ^= S1
         S ^= S2
         goto symbol2

symbol2: fi Q = Q2 && S = S2
           from symbol1 else symbol
         R <- (S1 . S2)
         goto act2

shft: from act1
      if Q = Q1 goto shft1 else shft3

shft1: from shft
       Q ^= Q1
       Q ^= Q2
       if R = 'LEFT
          goto left else right

left: from shft1
      if S_right = 'nil && S = 'BLANK
         goto left_1b else left_1p

left_1b: from left
         S ^= 'BLANK
         goto left1

left_1p: from left
         S_right <- (S . S_right)
         goto left1

left1: fi S_right = 'nil
          from left_1b else left_1p
       if S_left = 'nil
          goto left_2b else left_2p

left_2b: from left1
         S ^= 'BLANK
         goto left2

left_2p: from left1
         (S . S_left) <- S_left
         goto left2

left2: fi S_left = 'nil && S = 'BLANK
          from left_2b else left_2p
       goto shft2

right: from shft1
       if S_left = 'nil && S = 'BLANK
          goto right_1b else right_1p

right_1b: from right
          S ^= 'BLANK
          goto right1

right_1p: from right
          S_left <- (S . S_left)
          goto right1

right1: fi S_left = 'nil
           from right_1b else right_1p
        if S_right = 'nil goto right_2b
           else right_2p

right_2b: from right1
          S ^= 'BLANK
          goto right2

right_2p: from right1
          (S . S_right) <- S_right
          goto right2

right2: fi S_right = 'nil && S = 'BLANK
           from right_2b else right_2p
        goto shft2

shft2: fi R = 'LEFT
          from left2 else right2
       goto shft3

shft3: fi Q = Q2
          from shft2 else shft
       goto act2
