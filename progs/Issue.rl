(stat1 stat2 dyn) -> (stat1 stat2 dyn)
// Attempt to replicate an old issue that is now fixed

init: entry
      if dyn goto p1 else p2

p1: from init
    stat1 += '1
    goto p3

p2: from init
    stat1 += '2
    goto p3

p3: fi dyn from p1 else p2
    if dyn goto p4 else p5

p4: from p3
    stat2 += '1
    goto p6

p5: from p3
    stat2 += '2
    goto p6

p6: fi dyn from p4 else p5
    stat1 += dyn
    exit
