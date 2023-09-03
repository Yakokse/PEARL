// An effort to specialize by saying that Move and Write are mutually exclusive
init: entry
      S += BLANK // BLANK and SLASH should be defined in input
      if pc_max = 0 goto stop else loop

loop: fi Q = Start from init else check3
      if Q = End goto stop else check1

stop: fi pc_max = 0 from init else loop
      exit

check1: from loop 
        if Q = Q1[pc] && (S = S1[pc] || S1[pc] = SLASH)
            goto act1
            else check2

act1: from check1
      Q += Q2[pc] - Q1[pc]
      if S1[pc] = SLASH goto move else write

act2: fi S1[pc] = SLASH from move1 else write
      goto check2

check2: fi Q = Q2[pc] && (S = S2[pc] || S1[pc] = SLASH) from act2 else check1
        pc += 1
        if pc = pc_max goto clear else check3

clear: from check2
       pc -= pc_max
       goto check3

check3: fi pc = 0 from clear else check2
        goto loop

write: from act1
       S += S2[pc] - S1[pc]
       goto act2

move: from act1
      if S2[pc] = LEFT goto left else right

left: from move
      if empty S_right && S = BLANK goto left_1b else left_1p

left_1b: from left // MERGE? 1
         S -= BLANK
         goto left1

left_1p: from left
         push S S_right
         goto left1

left1: fi empty S_right from left_1b else left_1p
       if empty S_left goto left_2b else left_2p

left_2b: from left1 // MERGE? 2
         S += BLANK
         goto left2

left_2p: from left1
         pop S S_left
         goto left2

left2: fi empty S_left && S = BLANK from left_2b else left_2p
       goto move1

right: from move
       if empty S_left && S = BLANK goto right_1b else right_1p

right_1b: from right // MERGE? 1
          S -= BLANK
          goto right1

right_1p: from right
          push S S_left
          goto right1

right1: fi empty S_left from right_1b else right_1p
        if empty S_right goto right_2b else right_2p

right_2b: from right1 // MERGE? 2
          S += BLANK
          goto right2

right_2p: from right1
          pop S S_right
          goto right2

right2: fi empty S_right && S = BLANK from right_2b else right_2p
        goto move1

move1: fi S2[pc] = LEFT from left2 else right2
       goto act2
