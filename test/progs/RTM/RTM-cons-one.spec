// Inner variables
Q = 'nil
Q1 = 'nil
S1 = 'nil
S2 = 'nil
Q2 = 'nil
RulesRev = 'nil
Rules = 'nil
Rule = 'nil

// Describe the TM
// Cons 1 to any input
// Example: _101 --> _1101
Start = '1
End = '4
pc_max = '3
Transitions = 
 '((1 . (BLANK . (1     . 2))) . 
  ((2 . (SLASH . (LEFT  . 3))) . 
  ((3 . (BLANK . (BLANK . 4))) . 
   nil))))))))

// Tape for full specialization
//S = 'nil
//S_left = 'nil
//S_right = '(1 . (0 . (1 . nil)))
