// Describe the TM
// Flip bits in string (0|1)*
// Example: _101 --> _010
// RTM procedure entry pattern: (Start.(End.(Rules.S_right)))
'(1.
 (6.
 (((1 . (BLANK . (BLANK . 2))) .
  ((2 . (SLASH . (RIGHT . 3))) .
  ((3 . (0 .     (1     . 2))) .
  ((3 . (1 .     (0     . 2))) .
  ((3 . (BLANK . (BLANK . 4))) .
  ((4 . (SLASH . (LEFT  . 5))) .
  ((5 . (0 .     (0     . 4))) .
  ((5 . (1 .     (1     . 4))) .
  ((5 . (BLANK . (BLANK . 6))) .
   nil))))))))).
 (1 . (0 . (1 . nil)))
)))