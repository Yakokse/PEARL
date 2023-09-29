init: entry
      pc ^= '0
      Q ^= Start
      if pc_max = '0 goto stop else loop

loop: fi Q = Start from init else act4
      if Q = End goto stop else act1

stop: fi pc_max = '0 from init else loop
      exit

act1: from loop 
      Rule ^= Transitions # pc
      (Q1 . (S1 . (S2 . Q2))) <- Rule
      if Q = Q1 && S = S1 goto write else act2

write: from act1
       Q ^= Q1
       Q ^= Q2
       S ^= S1
       S ^= S2
       goto act2

act2: fi Q = Q2 && S = S2 from write else act1
      if Q = Q1 && S1 = 'SLASH goto move else act3

act3: fi Q = Q2 && S1 = 'SLASH from move1 else act2
      Rule <- (Q1 . (S1 . (S2 . Q2)))
      Rule ^= Transitions # pc
      pc += '1
      if pc = pc_max goto clear else act4

clear: from act3
       pc -= pc_max
       goto act4

act4: fi pc = '0 from clear else act3
      goto loop

move: from act2
      Q += Q2 - Q1
      if S2 = 'LEFT goto left else right

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
          S += 'BLANK
          goto right2

right_2p: from right1
          (S . S_right) <- S_right
          goto right2

right2: fi S_right = 'nil && S = 'BLANK from right_2b else right_2p
        goto move1

move1: fi S2 = 'LEFT from left2 else right2
       goto act3
