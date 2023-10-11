// Describe the TM
Start = '1
End = '6
Transitions = 
 '((1 . (BLANK . (BLANK . 2))) . 
  ((2 . (SLASH . (RIGHT . 3))) . 
  ((3 . (0 .     (1     . 4))) . 
  ((3 . (1 .     (0     . 2))) . 
  ((3 . (BLANK . (BLANK . 4))) . 
  ((4 . (SLASH . (LEFT  . 5))) . 
  ((5 . (0 .     (0     . 4))) . 
  ((5 . (BLANK . (BLANK . 6))) . 
   nil))))))))

// Tape for full specialization
//S = 'BLANK
//S_left = 'nil
//S_right = '(1 . (1 . (0 .  nil)))
