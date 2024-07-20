// Describe the TM
Start = '6
Final = '1

Rules =
 '((6 . ((BLANK . BLANK) . 5)) .
  ((4 . ((0     . 0    ) . 5)) .
  ((5 . (RIGHT           . 4)) .
  ((4 . ((BLANK . BLANK) . 3)) .
  ((2 . ((0     . 1    ) . 3)) .
  ((4 . ((1     . 0    ) . 3)) .
  ((3 . (LEFT            . 2)) .
  ((2 . ((BLANK . BLANK) . 1)) .
   nil))))))))

// Tape for full specialization
// S_right = '(0 . (0 . (1 .  nil)))
