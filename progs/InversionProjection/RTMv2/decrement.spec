// Describe the TM
Start = '1
End = '6
Rules =
 '((1 . ((BLANK . BLANK) . 2)) .
  ((2 . (RIGHT           . 3)) .
  ((3 . ((0 .     1    ) . 4)) .
  ((3 . ((1 .     0    ) . 2)) .
  ((3 . ((BLANK . BLANK) . 4)) .
  ((4 . (LEFT            . 5)) .
  ((5 . ((0 .     0    ) . 4)) .
  ((5 . ((BLANK . BLANK) . 6)) .
   nil))))))))

// Tape for full specialization
S_right = '(0 . (0 . (1 .  nil)))
