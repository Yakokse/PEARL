'(
(2 . (0 . (nil.nil))) .
(
//State transitions
((1 . (SLASH . (RIGHT . 2))) . //next
((2 . (1 .     (0     . 3))) . //toggle 1
   nil)).
//Involution
((1 . 4) . ((2 . 3) . ((3. 2). ((4.1).nil))))
))

// Input pattern
// (PartialRule.(Rules.Involution))
//Simple involution
//Two rules define the rest:
//(1 . (SLASH . (RIGHT . 2))) . //next
//(2 . (1 .     (0     . 3))) //toggle 1
//Since initial involuted must equal end, we have 1 <-> 4 and 2 <-> 3 thus
//(2 . (SLASH . (LEFT . 3))) . //prev (involuted next)
//(2 . (0 .     (1     . 3))) //toggle 2 (involuted toggle 1)
