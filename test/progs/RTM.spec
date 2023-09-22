// Symbols and required values
BLANK = 32
SLASH = 47
LEFT = 60
RIGHT = 62
S = 0
//S_left = ()
//S_right = (1, 1, 0)

// Starting state, both numbers must be equal
Q = 1
Start = 1

// End state and state machine definition
End = 6
pc = 0
pc_max = 8
Q1 = [ 1,  2, 3, 3,  3,  4, 5,  5]
S1 = [32, 47, 0, 1, 32, 47, 0, 32]
S2 = [32, 62, 1, 0, 32, 60, 0, 32]
Q2 = [ 2,  3, 4, 2,  4,  5, 4,  6]

// Q = 2, pc = 1
//- Specializing
//"loop" :: Q:(2) pc:(1)
//"act1" :: Q:(2) pc:(1)
//"write":: Q:(2) pc:(1) <- This should never happen
//"act2" :: Q:(3) pc:(1)
//"act3" :: Q:(3) pc:(1)
//Error while specializing: Invalid jump during elimination from "act2"

// Problem: Dynamic if-gotos lead to blocks being
// specialized that were never supposed to
// Solution: somehow strengthen the program
// or make it so the specializer does not crash and burn

// If specializer fails to generate some blocks then post process
// can remove the newly jumpless blocks