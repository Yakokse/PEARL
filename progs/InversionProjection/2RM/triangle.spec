// Calculate the n'th triangle number (sum of 1 to n)

Prog = '((Reg . Swap) . // swap
        ((Reg . Sum) . // y += x
        ((Inv . Swap) . // swap
        ((Inv . 1) . // x -= 1
        nil))))

// Invert
Y = '55
