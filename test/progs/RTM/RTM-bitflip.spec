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
// Flip bits in string (0|1)*
// Example: _101 --> _010
Start = '1
End = '6
pc_max = '9
Transitions = 
 '((1 . (BLANK . (BLANK . 2))) . 
  ((2 . (SLASH . (RIGHT . 3))) . 
  ((3 . (0 .     (1     . 2))) . 
  ((3 . (1 .     (0     . 2))) . 
  ((3 . (BLANK . (BLANK . 4))) . 
  ((4 . (SLASH . (LEFT  . 5))) . 
  ((5 . (0 .     (0     . 4))) . 
  ((5 . (1 .     (1     . 4))) . 
  ((5 . (BLANK . (BLANK . 6))) . 
   nil))))))))

// Tape for full specialization
//S = 'nil
//S_left = 'nil
//S_right = '(1 . (0 . (1 . nil)))
