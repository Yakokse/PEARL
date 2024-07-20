// Describe the TM
// Cons 1 to any input
// Example: _101 --> _1101
Start = '1
Final = '4
Rules =
 '((1 . ((BLANK . 1    ) . 2)) .
  ((2 . (LEFT            . 3)) .
  ((3 . ((BLANK . BLANK) . 4)) .
   nil)))

// Tape for full specialization
// S_right = '(1 . (0 . (1 . nil)))
