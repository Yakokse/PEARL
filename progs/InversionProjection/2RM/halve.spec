// Halve a number

// To invert, we need to also swap references between X and Y
// General way is to add swap to start and end (and remove if exist already)

Prog = '((Inv . 2) .
        ((Reg . Swap) .
        ((Reg . 1) .
        ((Inv . Swap) .
        nil))))

// Fully spec
X = '20
