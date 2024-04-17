// Double a number

Prog = '((Inv . 1) . // x -= 1
        ((Inv . Swap) . // swap
        ((Reg . 2) . // y += 2 (in x)
        ((Reg . Swap) . // swap
        nil))))

// Inverted
Y = '20
