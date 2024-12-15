// Describe the TM
// Replace 0 by 1 in string 0*
// Example: _000 --> _111
// RTM procedure entry pattern: (Start.(End.(Rules.S_right)))
'(1.
 (6.
 (((1 . (BLANK . (BLANK . 2))) .
  ((2 . (SLASH . (RIGHT . 3))) .
  ((3 . (0 .     (1     . 2))) .
  ((3 . (BLANK . (BLANK . 4))) .
  ((4 . (SLASH . (LEFT  . 5))) .
  ((5 . (1 .     (1     . 4))) .
  ((5 . (BLANK . (BLANK . 6))) .
   nil))))))).
 (0 . (0 . (0 . nil)))
 )))
