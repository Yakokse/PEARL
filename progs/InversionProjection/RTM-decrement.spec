// Describe the TM
Start = '6
End = '1

// Inverted rules
//Rules =
// '((2 . (BLANK . (BLANK . 1))) .
//  ((3 . (SLASH . (LEFT  . 2))) .
//  ((4 . (1 .     (0     . 3))) .
//  ((2 . (0 .     (1     . 3))) .
//  ((4 . (BLANK . (BLANK . 3))) .
//  ((5 . (SLASH . (RIGHT . 4))) .
//  ((4 . (0 .     (0     . 5))) .
//  ((6 . (BLANK . (BLANK . 5))) .
//   nil))))))))

// Inverted rules + order
Rules =
 '((6 . (BLANK . (BLANK . 5))) .
  ((4 . (0 .     (0     . 5))) .
  ((5 . (SLASH . (RIGHT . 4))) .
  ((4 . (BLANK . (BLANK . 3))) .
  ((2 . (0 .     (1     . 3))) .
  ((4 . (1 .     (0     . 3))) .
  ((3 . (SLASH . (LEFT  . 2))) .
  ((2 . (BLANK . (BLANK . 1))) .
   nil))))))))
